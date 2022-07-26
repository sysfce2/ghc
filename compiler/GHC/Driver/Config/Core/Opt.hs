{-
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998

\section[SimplCore]{Configuration of the driver for simplifying @Core@ programs}
-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE TupleSections #-}

module GHC.Driver.Config.Core.Opt ( getCoreToDo ) where

import GHC.Prelude

import GHC.Driver.Session
import GHC.Driver.Config.Core.Opt.CallerCC ( initCallerCCOpts )
import GHC.Driver.Config.Core.Opt.SpecConstr ( initSpecConstrOpts )
import GHC.Driver.Config.Core.Opt.LiberateCase ( initLiberateCaseOpts )
import GHC.Driver.Config.Core.Opt.Simplify ( initSimplifyOpts, initSimplMode, initGentleSimplMode )
import GHC.Platform.Ways  ( hasWay, Way(WayProf) )

import GHC.Core.Opt.Config ( CoreToDo(..) )
import GHC.Core.Opt.Utils        ( FloatOutSwitches(..) )

import GHC.Types.Basic
import GHC.Types.Var ( Var )

import qualified GHC.LanguageExtensions as LangExt

{-
************************************************************************
*                                                                      *
           Generating the main optimisation pipeline
*                                                                      *
************************************************************************
-}

getCoreToDo :: DynFlags -> [Var] -> [CoreToDo]
getCoreToDo dflags extra_vars
  = flatten_todos core_todo
  where
    phases        = simplPhases        dflags
    max_iter      = maxSimplIterations dflags
    platform      = targetPlatform     dflags
    rule_check    = ruleCheck          dflags
    const_fold    = gopt Opt_CoreConstantFolding          dflags
    call_arity    = gopt Opt_CallArity                    dflags
    exitification = gopt Opt_Exitification                dflags
    strictness    = gopt Opt_Strictness                   dflags
    full_laziness = gopt Opt_FullLaziness                 dflags
    do_specialise = gopt Opt_Specialise                   dflags
    do_float_in   = gopt Opt_FloatIn                      dflags
    cse           = gopt Opt_CSE                          dflags
    spec_constr   = gopt Opt_SpecConstr                   dflags
    liberate_case = gopt Opt_LiberateCase                 dflags
    late_dmd_anal = gopt Opt_LateDmdAnal                  dflags
    late_specialise = gopt Opt_LateSpecialise             dflags
    static_args   = gopt Opt_StaticArgumentTransformation dflags
    rules_on      = gopt Opt_EnableRewriteRules           dflags
    ww_on         = gopt Opt_WorkerWrapper                dflags
    static_ptrs   = xopt LangExt.StaticPointers           dflags
    profiling     = ways dflags `hasWay` WayProf

    do_presimplify = do_specialise -- TODO: any other optimizations benefit from pre-simplification?
    do_simpl3      = const_fold || rules_on -- TODO: any other optimizations benefit from three-phase simplification?

    maybe_rule_check phase = runMaybe rule_check (CoreDoRuleCheck phase)

    maybe_strictness_before (Phase phase)
      | phase `elem` strictnessBefore dflags = CoreDoDemand
    maybe_strictness_before _
      = CoreDoNothing

    simpl_phase phase name iter
      = CoreDoPasses
      $   [ maybe_strictness_before phase
          , CoreDoSimplify $ initSimplifyOpts dflags extra_vars iter
                             (initSimplMode dflags phase name)
          , maybe_rule_check phase ]

    -- Run GHC's internal simplification phase, after all rules have run.
    -- See Note [Compiler phases] in GHC.Types.Basic
    simplify name = simpl_phase FinalPhase name max_iter

    -- initial simplify: mk specialiser happy: minimum effort please
    -- See Note [Inline in InitialPhase]
    -- See Note [RULEs enabled in InitialPhase]
    simpl_gently = CoreDoSimplify $ initSimplifyOpts dflags extra_vars max_iter
                                    (initGentleSimplMode dflags)

    dmd_cpr_ww = [CoreDoDemand, CoreDoCpr] ++
      if ww_on then [CoreDoWorkerWrapper]
               else []


    demand_analyser = (CoreDoPasses (
                           dmd_cpr_ww ++
                           [simplify "post-worker-wrapper"]
                           ))

    -- Static forms are moved to the top level with the FloatOut pass.
    -- See Note [Grand plan for static forms] in GHC.Iface.Tidy.StaticPtrTable.
    static_ptrs_float_outwards =
      runWhen static_ptrs $ CoreDoPasses
        [ simpl_gently -- Float Out can't handle type lets (sometimes created
                       -- by simpleOptPgm via mkParallelBindings)
        , CoreDoFloatOutwards FloatOutSwitches
          { floatOutLambdas   = Just 0
          , floatOutConstants = True
          , floatOutOverSatApps = False
          , floatToTopLevelOnly = True
          }
        ]

    add_caller_ccs =
        runWhen (profiling && not (null $ callerCcFilters dflags)) $
          CoreAddCallerCcs (initCallerCCOpts dflags)

    add_late_ccs =
        runWhen (profiling && gopt Opt_ProfLateCcs dflags) $
          CoreAddLateCcs (gopt Opt_ProfCountEntries dflags)

    core_todo =
     [
    -- We want to do the static argument transform before full laziness as it
    -- may expose extra opportunities to float things outwards. However, to fix
    -- up the output of the transformation we need at do at least one simplify
    -- after this before anything else
        runWhen static_args (CoreDoPasses [ simpl_gently, CoreDoStaticArgs ]),

        -- initial simplify: mk specialiser happy: minimum effort please
        runWhen do_presimplify simpl_gently,

        -- Specialisation is best done before full laziness
        -- so that overloaded functions have all their dictionary lambdas manifest
        runWhen do_specialise CoreDoSpecialising,

        if full_laziness then
           CoreDoFloatOutwards FloatOutSwitches {
                                 floatOutLambdas   = Just 0,
                                 floatOutConstants = True,
                                 floatOutOverSatApps = False,
                                 floatToTopLevelOnly = False }
                -- Was: gentleFloatOutSwitches
                --
                -- I have no idea why, but not floating constants to
                -- top level is very bad in some cases.
                --
                -- Notably: p_ident in spectral/rewrite
                --          Changing from "gentle" to "constantsOnly"
                --          improved rewrite's allocation by 19%, and
                --          made 0.0% difference to any other nofib
                --          benchmark
                --
                -- Not doing floatOutOverSatApps yet, we'll do
                -- that later on when we've had a chance to get more
                -- accurate arity information.  In fact it makes no
                -- difference at all to performance if we do it here,
                -- but maybe we save some unnecessary to-and-fro in
                -- the simplifier.
        else
           -- Even with full laziness turned off, we still need to float static
           -- forms to the top level. See Note [Grand plan for static forms] in
           -- GHC.Iface.Tidy.StaticPtrTable.
           static_ptrs_float_outwards,

        -- Run the simplier phases 2,1,0 to allow rewrite rules to fire
        runWhen do_simpl3
            (CoreDoPasses $ [ simpl_phase (Phase phase) "main" max_iter
                            | phase <- [phases, phases-1 .. 1] ] ++
                            [ simpl_phase (Phase 0) "main" (max max_iter 3) ]),
                -- Phase 0: allow all Ids to be inlined now
                -- This gets foldr inlined before strictness analysis

                -- At least 3 iterations because otherwise we land up with
                -- huge dead expressions because of an infelicity in the
                -- simplifier.
                --      let k = BIG in foldr k z xs
                -- ==>  let k = BIG in letrec go = \xs -> ...(k x).... in go xs
                -- ==>  let k = BIG in letrec go = \xs -> ...(BIG x).... in go xs
                -- Don't stop now!

        runWhen do_float_in (CoreDoFloatInwards platform),
            -- Run float-inwards immediately before the strictness analyser
            -- Doing so pushes bindings nearer their use site and hence makes
            -- them more likely to be strict. These bindings might only show
            -- up after the inlining from simplification.  Example in fulsom,
            -- Csg.calc, where an arg of timesDouble thereby becomes strict.

        runWhen call_arity $ CoreDoPasses
            [ CoreDoCallArity
            , simplify "post-call-arity"
            ],

        -- Strictness analysis
        runWhen strictness demand_analyser,

        runWhen exitification CoreDoExitify,
            -- See Note [Placement of the exitification pass]

        runWhen full_laziness $
           CoreDoFloatOutwards FloatOutSwitches {
                                 floatOutLambdas     = floatLamArgs dflags,
                                 floatOutConstants   = True,
                                 floatOutOverSatApps = True,
                                 floatToTopLevelOnly = False },
                -- nofib/spectral/hartel/wang doubles in speed if you
                -- do full laziness late in the day.  It only happens
                -- after fusion and other stuff, so the early pass doesn't
                -- catch it.  For the record, the redex is
                --        f_el22 (f_el21 r_midblock)


        runWhen cse CoreCSE,
                -- We want CSE to follow the final full-laziness pass, because it may
                -- succeed in commoning up things floated out by full laziness.
                -- CSE used to rely on the no-shadowing invariant, but it doesn't any more

        runWhen do_float_in (CoreDoFloatInwards platform),

        simplify "final",  -- Final tidy-up

        maybe_rule_check FinalPhase,

        --------  After this we have -O2 passes -----------------
        -- None of them run with -O

                -- Case-liberation for -O2.  This should be after
                -- strictness analysis and the simplification which follows it.
        runWhen liberate_case $ CoreDoPasses
           [ CoreLiberateCase (initLiberateCaseOpts dflags)
           , simplify "post-liberate-case" ],
           -- Run the simplifier after LiberateCase to vastly
           -- reduce the possibility of shadowing
           -- Reason: see Note [Shadowing] in GHC.Core.Opt.SpecConstr

        runWhen spec_constr $ CoreDoPasses
           [ CoreDoSpecConstr (initSpecConstrOpts dflags)
           , simplify "post-spec-constr"],
           -- See Note [Simplify after SpecConstr]

        maybe_rule_check FinalPhase,

        runWhen late_specialise $ CoreDoPasses
           [ CoreDoSpecialising, simplify "post-late-spec"],

        -- LiberateCase can yield new CSE opportunities because it peels
        -- off one layer of a recursive function (concretely, I saw this
        -- in wheel-sieve1), and I'm guessing that SpecConstr can too
        -- And CSE is a very cheap pass. So it seems worth doing here.
        runWhen ((liberate_case || spec_constr) && cse) $ CoreDoPasses
           [ CoreCSE, simplify "post-final-cse" ],

        ---------  End of -O2 passes --------------

        runWhen late_dmd_anal $ CoreDoPasses (
            dmd_cpr_ww ++ [simplify "post-late-ww"]
          ),

        -- Final run of the demand_analyser, ensures that one-shot thunks are
        -- really really one-shot thunks. Only needed if the demand analyser
        -- has run at all. See Note [Final Demand Analyser run] in GHC.Core.Opt.DmdAnal
        -- It is EXTREMELY IMPORTANT to run this pass, otherwise execution
        -- can become /exponentially/ more expensive. See #11731, #12996.
        runWhen (strictness || late_dmd_anal) CoreDoDemand,

        maybe_rule_check FinalPhase,

        add_caller_ccs,
        add_late_ccs
     ]

    -- Remove 'CoreDoNothing' and flatten 'CoreDoPasses' for clarity.
    flatten_todos [] = []
    flatten_todos (CoreDoNothing : rest) = flatten_todos rest
    flatten_todos (CoreDoPasses passes : rest) =
      flatten_todos passes ++ flatten_todos rest
    flatten_todos (todo : rest) = todo : flatten_todos rest

-- The core-to-core pass ordering is derived from the DynFlags:
runWhen :: Bool -> CoreToDo -> CoreToDo
runWhen True  do_this = do_this
runWhen False _       = CoreDoNothing

runMaybe :: Maybe a -> (a -> CoreToDo) -> CoreToDo
runMaybe (Just x) f = f x
runMaybe Nothing  _ = CoreDoNothing

{- Note [Inline in InitialPhase]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In GHC 8 and earlier we did not inline anything in the InitialPhase. But that is
confusing for users because when they say INLINE they expect the function to inline
right away.

So now we do inlining immediately, even in the InitialPhase, assuming that the
Id's Activation allows it.

This is a surprisingly big deal. Compiler performance improved a lot
when I made this change:

   perf/compiler/T5837.run            T5837 [stat too good] (normal)
   perf/compiler/parsing001.run       parsing001 [stat too good] (normal)
   perf/compiler/T12234.run           T12234 [stat too good] (optasm)
   perf/compiler/T9020.run            T9020 [stat too good] (optasm)
   perf/compiler/T3064.run            T3064 [stat too good] (normal)
   perf/compiler/T9961.run            T9961 [stat too good] (normal)
   perf/compiler/T13056.run           T13056 [stat too good] (optasm)
   perf/compiler/T9872d.run           T9872d [stat too good] (normal)
   perf/compiler/T783.run             T783 [stat too good] (normal)
   perf/compiler/T12227.run           T12227 [stat too good] (normal)
   perf/should_run/lazy-bs-alloc.run  lazy-bs-alloc [stat too good] (normal)
   perf/compiler/T1969.run            T1969 [stat too good] (normal)
   perf/compiler/T9872a.run           T9872a [stat too good] (normal)
   perf/compiler/T9872c.run           T9872c [stat too good] (normal)
   perf/compiler/T9872b.run           T9872b [stat too good] (normal)
   perf/compiler/T9872d.run           T9872d [stat too good] (normal)

Note [RULEs enabled in InitialPhase]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
RULES are enabled when doing "gentle" simplification in InitialPhase,
or with -O0.  Two reasons:

  * We really want the class-op cancellation to happen:
        op (df d1 d2) --> $cop3 d1 d2
    because this breaks the mutual recursion between 'op' and 'df'

  * I wanted the RULE
        lift String ===> ...
    to work in Template Haskell when simplifying
    splices, so we get simpler code for literal strings

But watch out: list fusion can prevent floating.  So use phase control
to switch off those rules until after floating.

Note [Simplify after SpecConstr]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We want to run the simplifier after SpecConstr, and before late-Specialise,
for two reasons, both shown up in test perf/compiler/T16473,
with -O2 -flate-specialise

1.  I found that running late-Specialise after SpecConstr, with no
    simplification in between meant that the carefullly constructed
    SpecConstr rule never got to fire.  (It was something like
          lvl = f a   -- Arity 1
          ....g lvl....
    SpecConstr specialised g for argument lvl; but Specialise then
    specialised lvl = f a to lvl = $sf, and inlined. Or something like
    that.)

2.  Specialise relies on unfoldings being available for top-level dictionary
    bindings; but SpecConstr kills them all!  The Simplifer restores them.

This extra run of the simplifier has a cost, but this is only with -O2.

-}