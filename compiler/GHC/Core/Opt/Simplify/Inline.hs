{-
(c) The University of Glasgow 2006
(c) The AQUA Project, Glasgow University, 1994-1998

This module contains inlining logic used by the simplifier.
-}


{-# LANGUAGE BangPatterns #-}

module GHC.Core.Opt.Simplify.Inline (
        ArgSummary(..),

        -- * Cheap and cheerful inlining checks.
        couldBeSmallEnoughToInline,
        smallEnoughToInline,

        -- * The smart inlining decisions are made by callSiteInline
        interestingArg,
        callSiteInline, CallCtxt(..),
    ) where

import GHC.Prelude

import GHC.Driver.Flags

import GHC.Core
import GHC.Core.Unfold
import GHC.Types.Id
import GHC.Utils.Logger
import GHC.Utils.Misc
import GHC.Utils.Outputable
import GHC.Types.Name

import Data.List (isPrefixOf)

import GHC.Core.TyCon (isClassTyCon)

import GHC.Types.Basic

import GHC.Types.Unique.FM
import GHC.Core.Opt.Simplify.Env
import GHC.Types.Literal
import GHC.Core.DataCon
import GHC.Types.Var
import GHC.Utils.Panic.Plain (assert)
import GHC.Types.Tickish
{-
************************************************************************
*                                                                      *
\subsection[considerUnfolding]{Given all the info, do (not) do the unfolding}
*                                                                      *
************************************************************************

We use 'couldBeSmallEnoughToInline' to avoid exporting inlinings that
we ``couldn't possibly use'' on the other side.  Can be overridden w/
flaggery.  Just the same as smallEnoughToInline, except that it has no
actual arguments.
-}

couldBeSmallEnoughToInline :: UnfoldingOpts -> Int -> CoreExpr -> Bool
couldBeSmallEnoughToInline opts threshold rhs
  = case sizeExpr opts threshold [] body of
       TooBig -> False
       _      -> True
  where
    (_, body) = collectBinders rhs

----------------
smallEnoughToInline :: UnfoldingOpts -> Unfolding -> Bool
smallEnoughToInline opts (CoreUnfolding {uf_guidance = guidance})
  = case guidance of
       UnfIfGoodArgs {ug_size = size} -> size <= unfoldingUseThreshold opts
       UnfWhen {} -> True
       UnfNever   -> False
smallEnoughToInline _ _
  = False


{-
************************************************************************
*                                                                      *
\subsection{callSiteInline}
*                                                                      *
************************************************************************

This is the key function.  It decides whether to inline a variable at a call site

callSiteInline is used at call sites, so it is a bit more generous.
It's a very important function that embodies lots of heuristics.
A non-WHNF can be inlined if it doesn't occur inside a lambda,
and occurs exactly once or
    occurs once in each branch of a case and is small

If the thing is in WHNF, there's no danger of duplicating work,
so we can inline if it occurs once, or is small

NOTE: we don't want to inline top-level functions that always diverge.
It just makes the code bigger.  Tt turns out that the convenient way to prevent
them inlining is to give them a NOINLINE pragma, which we do in
StrictAnal.addStrictnessInfoToTopId
-}

{-
************************************************************************
*                                                                      *
\subsection{callSiteInline}
*                                                                      *
************************************************************************

This is the key function.  It decides whether to inline a variable at a call site

callSiteInline is used at call sites, so it is a bit more generous.
It's a very important function that embodies lots of heuristics.
A non-WHNF can be inlined if it doesn't occur inside a lambda,
and occurs exactly once or
    occurs once in each branch of a case and is small

If the thing is in WHNF, there's no danger of duplicating work,
so we can inline if it occurs once, or is small

NOTE: we don't want to inline top-level functions that always diverge.
It just makes the code bigger.  Tt turns out that the convenient way to prevent
them inlining is to give them a NOINLINE pragma, which we do in
StrictAnal.addStrictnessInfoToTopId
-}

{-
************************************************************************
*                                                                      *
\subsection{callSiteInline}
*                                                                      *
************************************************************************

This is the key function.  It decides whether to inline a variable at a call site

callSiteInline is used at call sites, so it is a bit more generous.
It's a very important function that embodies lots of heuristics.
A non-WHNF can be inlined if it doesn't occur inside a lambda,
and occurs exactly once or
    occurs once in each branch of a case and is small

If the thing is in WHNF, there's no danger of duplicating work,
so we can inline if it occurs once, or is small

NOTE: we don't want to inline top-level functions that always diverge.
It just makes the code bigger.  Tt turns out that the convenient way to prevent
them inlining is to give them a NOINLINE pragma, which we do in
StrictAnal.addStrictnessInfoToTopId
-}



callSiteInline :: Logger
               -> UnfoldingOpts
               -> Int                   -- Case depth
               -> Id                    -- The Id
               -> Bool                  -- True <=> unfolding is active
               -> Bool                  -- True if there are no arguments at all (incl type args)
               -> [ArgSummary]          -- One for each value arg; True if it is interesting
               -> CallCtxt              -- True <=> continuation is interesting
               -> Maybe CoreExpr        -- Unfolding, if any
callSiteInline logger !opts !case_depth id active_unfolding lone_variable arg_infos cont_info
  = case idUnfolding id of
      -- idUnfolding checks for loop-breakers, returning NoUnfolding
      -- Things with an INLINE pragma may have an unfolding *and*
      -- be a loop breaker  (maybe the knot is not yet untied)
        CoreUnfolding { uf_tmpl = unf_template
                      , uf_is_work_free = is_wf
                      , uf_guidance = guidance, uf_expandable = is_exp }
          | active_unfolding -> tryUnfolding logger opts case_depth id lone_variable
                                    arg_infos cont_info unf_template
                                    is_wf is_exp guidance
          | otherwise -> traceInline logger opts id "Inactive unfolding:" (ppr id) Nothing
        NoUnfolding      -> Nothing
        BootUnfolding    -> Nothing
        OtherCon {}      -> Nothing
        DFunUnfolding {} -> Nothing     -- Never unfold a DFun

-- | Report the inlining of an identifier's RHS to the user, if requested.
traceInline :: Logger -> UnfoldingOpts -> Id -> String -> SDoc -> a -> a
traceInline logger !opts inline_id str doc result
  -- We take care to ensure that doc is used in only one branch, ensuring that
  -- the simplifier can push its allocation into the branch. See Note [INLINE
  -- conditional tracing utilities].
  | enable    = logTraceMsg logger str doc result
  | otherwise = result
  where
    enable
      | logHasDumpFlag logger Opt_D_dump_verbose_inlinings
      = True
      | Just prefix <- unfoldingReportPrefix opts
      = prefix `isPrefixOf` occNameString (getOccName inline_id)
      | otherwise
      = False
{-# INLINE traceInline #-} -- see Note [INLINE conditional tracing utilities]

{- Note [Avoid inlining into deeply nested cases]
   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Consider a function f like this:

  f arg1 arg2 =
    case ...
      ... -> g arg1
      ... -> g arg2

This function is small. So should be safe to inline.
However sometimes this doesn't quite work out like that.
Consider this code:

f1 arg1 arg2 ... = ...
    case _foo of
      alt1 -> ... f2 arg1 ...
      alt2 -> ... f2 arg2 ...

f2 arg1 arg2 ... = ...
    case _foo of
      alt1 -> ... f3 arg1 ...
      alt2 -> ... f3 arg2 ...

f3 arg1 arg2 ... = ...

... repeats up to n times. And then f1 is
applied to some arguments:

foo = ... f1 <interestingArgs> ...

Initially f2..fn are not interesting to inline so we don't.
However we see that f1 is applied to interesting args.
So it's an obvious choice to inline those:

foo =
    ...
      case _foo of
        alt1 -> ... f2 <interestingArg> ...
        alt2 -> ... f2 <interestingArg> ...

As a result we go and inline f2 both mentions of f2 in turn are now applied to interesting
arguments and f2 is small:

foo =
    ...
      case _foo of
        alt1 -> ... case _foo of
            alt1 -> ... f3 <interestingArg> ...
            alt2 -> ... f3 <interestingArg> ...

        alt2 -> ... case _foo of
            alt1 -> ... f3 <interestingArg> ...
            alt2 -> ... f3 <interestingArg> ...

The same thing happens for each binding up to f_n, duplicating the amount of inlining
done in each step. Until at some point we are either done or run out of simplifier
ticks/RAM. This pattern happened #18730.

To combat this we introduce one more heuristic when weighing inlining decision.
We keep track of a "case-depth". Which increases each time we look inside a case
expression with more than one alternative.

We then apply a penalty to inlinings based on the case-depth at which they would
be inlined. Bounding the number of inlinings in such a scenario.

The heuristic can be tuned in two ways:

* We can ignore the first n levels of case nestings for inlining decisions using
  -funfolding-case-threshold.
* The penalty grows linear with the depth. It's computed as size*(depth-threshold)/scaling.
  Scaling can be set with -funfolding-case-scaling.

Some guidance on setting these defaults:

* A low treshold (<= 2) is needed to prevent exponential cases from spiraling out of
  control. We picked 2 for no particular reason.
* Scaling the penalty by any more than 30 means the reproducer from
  T18730 won't compile even with reasonably small values of n. Instead
  it will run out of runs/ticks. This means to positively affect the reproducer
  a scaling <= 30 is required.
* A scaling of >= 15 still causes a few very large regressions on some nofib benchmarks.
  (+80% for gc/fulsom, +90% for real/ben-raytrace, +20% for spectral/fibheaps)
* A scaling of >= 25 showed no regressions on nofib. However it showed a number of
  (small) regression for compiler perf benchmarks.

The end result is that we are settling for a scaling of 30, with a threshold of 2.
This gives us minimal compiler perf regressions. No nofib runtime regressions and
will still avoid this pattern sometimes. This is a "safe" default, where we err on
the side of compiler blowup instead of risking runtime regressions.

For cases where the default falls short the flag can be changed to allow more/less inlining as
needed on a per-module basis.

-}

tryUnfolding :: Logger -> UnfoldingOpts -> Int -> Id -> Bool -> [ArgSummary] -> CallCtxt
             -> CoreExpr -> Bool -> Bool -> UnfoldingGuidance
             -> Maybe CoreExpr
tryUnfolding logger opts !case_depth id lone_variable
             arg_infos cont_info unf_template
             is_wf is_exp guidance
 = case guidance of
     UnfNever -> traceInline logger opts id str (text "UnfNever") Nothing

     UnfWhen { ug_arity = uf_arity, ug_unsat_ok = unsat_ok, ug_boring_ok = boring_ok }
        | enough_args && (boring_ok || some_benefit || unfoldingVeryAggressive opts)
                -- See Note [INLINE for small functions] (3)
        -> traceInline logger opts id str (mk_doc some_benefit empty True) (Just unf_template)
        | otherwise
        -> traceInline logger opts id str (mk_doc some_benefit empty False) Nothing
        where
          some_benefit = calc_some_benefit uf_arity
          enough_args = (n_val_args >= uf_arity) || (unsat_ok && n_val_args > 0)

     UnfIfGoodArgs { ug_args = arg_discounts, ug_res = res_discount, ug_size = size }
        | unfoldingVeryAggressive opts
        -> traceInline logger opts id str (mk_doc some_benefit extra_doc True) (Just unf_template)
        | is_wf && some_benefit && small_enough
        -> traceInline logger opts id str (mk_doc some_benefit extra_doc True) (Just unf_template)
        | otherwise
        -> traceInline logger opts id str (mk_doc some_benefit extra_doc False) Nothing
        where
          !some_benefit = calc_some_benefit (length arg_discounts)
          extra_doc = vcat [ text "case depth =" <+> int case_depth
                           , text "depth based penalty =" <+> int depth_penalty
                           , text "discounted size =" <+> int adjusted_size ]
          -- See Note [Avoid inlining into deeply nested cases]
          depth_treshold = unfoldingCaseThreshold opts
          depth_scaling = unfoldingCaseScaling opts
          depth_penalty | case_depth <= depth_treshold = 0
                        | otherwise       = (size * (case_depth - depth_treshold)) `div` depth_scaling
          !adjusted_size = size + depth_penalty - discount
          !small_enough = adjusted_size <= unfoldingUseThreshold opts
          discount = computeDiscount arg_discounts res_discount arg_infos cont_info

  where
    mk_doc some_benefit extra_doc yes_or_no
      = vcat [ text "arg infos" <+> ppr arg_infos
             , text "interesting continuation" <+> ppr cont_info
             , text "some_benefit" <+> ppr some_benefit
             , text "is exp:" <+> ppr is_exp
             , text "is work-free:" <+> ppr is_wf
             , text "guidance" <+> ppr guidance
             , extra_doc
             , text "ANSWER =" <+> if yes_or_no then text "YES" else text "NO"]

    ctx = log_default_dump_context (logFlags logger)
    str = "Considering inlining: " ++ showSDocOneLine ctx (ppr id)
    n_val_args = length arg_infos

           -- some_benefit is used when the RHS is small enough
           -- and the call has enough (or too many) value
           -- arguments (ie n_val_args >= arity). But there must
           -- be *something* interesting about some argument, or the
           -- result context, to make it worth inlining
    calc_some_benefit :: Arity -> Bool   -- The Arity is the number of args
                                         -- expected by the unfolding
    calc_some_benefit uf_arity
       | not saturated = interesting_args       -- Under-saturated
                                        -- Note [Unsaturated applications]
       | otherwise = interesting_args   -- Saturated or over-saturated
                  || interesting_call
      where
        saturated      = n_val_args >= uf_arity
        over_saturated = n_val_args > uf_arity
        interesting_args = any nonTrivArg arg_infos
                -- NB: (any nonTriv arg_infos) looks at the
                -- over-saturated args too which is "wrong";
                -- but if over-saturated we inline anyway.

        interesting_call
          | over_saturated
          = True
          | otherwise
          = case cont_info of
              CaseCtxt   -> not (lone_variable && is_exp)  -- Note [Lone variables]
              ValAppCtxt -> True                           -- Note [Cast then apply]
              RuleArgCtxt -> uf_arity > 0  -- See Note [RHS of lets]
              DiscArgCtxt -> uf_arity > 0  -- Note [Inlining in ArgCtxt]
              RhsCtxt NonRecursive
                          -> uf_arity > 0  -- See Note [RHS of lets]
              _other      -> False         -- See Note [Nested functions]


{- Note [RHS of lets]
~~~~~~~~~~~~~~~~~~~~~
When the call is the argument of a function with a RULE, or the RHS of a let,
we are a little bit keener to inline (in tryUnfolding).  For example
     f y = (y,y,y)
     g y = let x = f y in ...(case x of (a,b,c) -> ...) ...
We'd inline 'f' if the call was in a case context, and it kind-of-is,
only we can't see it.  Also
     x = f v
could be expensive whereas
     x = case v of (a,b) -> a
is patently cheap and may allow more eta expansion.

So, in `interesting_call` in `tryUnfolding`, we treat the RHS of a
/non-recursive/ let as not-totally-boring.  A /recursive/ let isn't
going be inlined so there is much less point.  Hence the (only reason
for the) RecFlag in RhsCtxt

Note [Unsaturated applications]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When a call is not saturated, we *still* inline if one of the
arguments has interesting structure.  That's sometimes very important.
A good example is the Ord instance for Bool in Base:

 Rec {
    $fOrdBool =GHC.Classes.D:Ord
                 @ Bool
                 ...
                 $cmin_ajX

    $cmin_ajX [Occ=LoopBreaker] :: Bool -> Bool -> Bool
    $cmin_ajX = GHC.Classes.$dmmin @ Bool $fOrdBool
  }

But the defn of GHC.Classes.$dmmin is:

  $dmmin :: forall a. GHC.Classes.Ord a => a -> a -> a
    {- Arity: 3, HasNoCafRefs, Strictness: SLL,
       Unfolding: (\ @ a $dOrd :: GHC.Classes.Ord a x :: a y :: a ->
                   case @ a GHC.Classes.<= @ a $dOrd x y of wild {
                     GHC.Types.False -> y GHC.Types.True -> x }) -}

We *really* want to inline $dmmin, even though it has arity 3, in
order to unravel the recursion.


Note [Things to watch]
~~~~~~~~~~~~~~~~~~~~~~
*   { y = I# 3; x = y `cast` co; ...case (x `cast` co) of ... }
    Assume x is exported, so not inlined unconditionally.
    Then we want x to inline unconditionally; no reason for it
    not to, and doing so avoids an indirection.

*   { x = I# 3; ....f x.... }
    Make sure that x does not inline unconditionally!
    Lest we get extra allocation.

Note [Nested functions]
~~~~~~~~~~~~~~~~~~~~~~~
At one time we treated a call of a non-top-level function as
"interesting" (regardless of how boring the context) in the hope
that inlining it would eliminate the binding, and its allocation.
Specifically, in the default case of interesting_call we had
   _other -> not is_top && uf_arity > 0

But actually postInlineUnconditionally does some of this and overall
it makes virtually no difference to nofib.  So I simplified away this
special case

Note [Cast then apply]
~~~~~~~~~~~~~~~~~~~~~~
Consider
   myIndex = __inline_me ( (/\a. <blah>) |> co )
   co :: (forall a. a -> a) ~ (forall a. T a)
     ... /\a.\x. case ((myIndex a) |> sym co) x of { ... } ...

We need to inline myIndex to unravel this; but the actual call (myIndex a) has
no value arguments.  The ValAppCtxt gives it enough incentive to inline.

Note [Inlining in ArgCtxt]
~~~~~~~~~~~~~~~~~~~~~~~~~~
The condition (arity > 0) here is very important, because otherwise
we end up inlining top-level stuff into useless places; eg
   x = I# 3#
   f = \y.  g x
This can make a very big difference: it adds 16% to nofib 'integer' allocs,
and 20% to 'power'.

At one stage I replaced this condition by 'True' (leading to the above
slow-down).  The motivation was test eyeball/inline1.hs; but that seems
to work ok now.

NOTE: arguably, we should inline in ArgCtxt only if the result of the
call is at least CONLIKE.  At least for the cases where we use ArgCtxt
for the RHS of a 'let', we only profit from the inlining if we get a
CONLIKE thing (modulo lets).

Note [Lone variables]   See also Note [Interaction of exprIsWorkFree and lone variables]
~~~~~~~~~~~~~~~~~~~~~   which appears below
The "lone-variable" case is important.  I spent ages messing about
with unsatisfactory variants, but this is nice.  The idea is that if a
variable appears all alone

        as an arg of lazy fn, or rhs    BoringCtxt
        as scrutinee of a case          CaseCtxt
        as arg of a fn                  ArgCtxt
AND
        it is bound to a cheap expression

then we should not inline it (unless there is some other reason,
e.g. it is the sole occurrence).  That is what is happening at
the use of 'lone_variable' in 'interesting_call'.

Why?  At least in the case-scrutinee situation, turning
        let x = (a,b) in case x of y -> ...
into
        let x = (a,b) in case (a,b) of y -> ...
and thence to
        let x = (a,b) in let y = (a,b) in ...
is bad if the binding for x will remain.

Another example: I discovered that strings
were getting inlined straight back into applications of 'error'
because the latter is strict.
        s = "foo"
        f = \x -> ...(error s)...

Fundamentally such contexts should not encourage inlining because, provided
the RHS is "expandable" (see Note [exprIsExpandable] in GHC.Core.Utils) the
context can ``see'' the unfolding of the variable (e.g. case or a
RULE) so there's no gain.

However, watch out:

 * Consider this:
        foo = \n. [n])  {-# INLINE foo #-}
        bar = foo 20    {-# INLINE bar #-}
        baz = \n. case bar of { (m:_) -> m + n }
   Here we really want to inline 'bar' so that we can inline 'foo'
   and the whole thing unravels as it should obviously do.  This is
   important: in the NDP project, 'bar' generates a closure data
   structure rather than a list.

   So the non-inlining of lone_variables should only apply if the
   unfolding is regarded as expandable; because that is when
   exprIsConApp_maybe looks through the unfolding.  Hence the "&&
   is_exp" in the CaseCtxt branch of interesting_call

 * Even a type application or coercion isn't a lone variable.
   Consider
        case $fMonadST @ RealWorld of { :DMonad a b c -> c }
   We had better inline that sucker!  The case won't see through it.

   For now, I'm treating treating a variable applied to types
   in a *lazy* context "lone". The motivating example was
        f = /\a. \x. BIG
        g = /\a. \y.  h (f a)
   There's no advantage in inlining f here, and perhaps
   a significant disadvantage.  Hence some_val_args in the Stop case

Note [Interaction of exprIsWorkFree and lone variables]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The lone-variable test says "don't inline if a case expression
scrutinises a lone variable whose unfolding is cheap".  It's very
important that, under these circumstances, exprIsConApp_maybe
can spot a constructor application. So, for example, we don't
consider
        let x = e in (x,x)
to be cheap, and that's good because exprIsConApp_maybe doesn't
think that expression is a constructor application.

In the 'not (lone_variable && is_wf)' test, I used to test is_value
rather than is_wf, which was utterly wrong, because the above
expression responds True to exprIsHNF, which is what sets is_value.

This kind of thing can occur if you have

        {-# INLINE foo #-}
        foo = let x = e in (x,x)

which Roman did.

Note [Minimum value discount]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We always give *some* benefit to value arguments.
A discount of 10 per arg because we replace the arguments
and another of 10 if it's some non-trivial value.
However when computing unfolding guidance we might have come to
the conclusion that certain argument values deservere little or no
discount. But we want to chance of inlining to only ever increase as
more is known about the argument to keep things more predictable. So
we always give at least 10 discount if the argument is a value. No matter
what the actual value is.
-}



computeDiscount :: [ArgDiscount] -> Int -> [ArgSummary] -> CallCtxt
                -> Int
computeDiscount arg_discounts !res_discount arg_infos cont_info

  = 10          -- Discount of 10 because the result replaces the call
                -- so we count 10 for the function itself

    + 10 * applied_arg_length
               -- Discount of 10 for each arg supplied,
               -- because the result replaces the call

    + total_arg_discount + res_discount'
  where
    (applied_arg_length,total_arg_discount) = zipWithSumLength arg_discounts arg_infos
    -- actual_arg_discounts = zipWith mk_arg_discount (arg_discounts) arg_infos
    -- total_arg_discount   = sum actual_arg_discounts

    -- See Note [Minimum value discount]
    mk_arg_discount :: ArgDiscount -> ArgSummary -> Int
    mk_arg_discount _        TrivArg    = 0
    mk_arg_discount _        NonTrivArg = 10
    mk_arg_discount NoSeqUse    _       = 10
    mk_arg_discount discount ValueArg   = max 10 (ad_seq_discount discount)
    mk_arg_discount (DiscSeq seq_discount con_discounts) (ConArg con args)

      -- There is a discount specific to this constructor, use that.
      | Just (ConDiscount _ branch_dc arg_discounts) <- lookupUFM con_discounts con
      = max 10 $ max seq_discount (branch_dc + (sum $ zipWith mk_arg_discount arg_discounts args))

      -- Otherwise give it the generic seq discount
      | otherwise = max 10 seq_discount
    mk_arg_discount (SomeArgUse d) ConArg{} = max 10 d
    mk_arg_discount (FunDisc d _) (ConArg{})
      -- How can this arise? With dictionary constructors for example.
      -- We see C:Show foo bar and give it a FunDisc for being applied
      -- like a function.
      -- But when constructing ArgSummaries we treat it as constructor
      -- since well it is one. This is harmless, but a bit odd for sure.
      -- We just treat it like any other boring ValueArg here.
      = -- pprTrace "Function discount for con arg" (ppr arg_infos)
        max 10 d

    -- zipWithSumLength xs ys = (length $ zip xs ys, sum $ zipWith _ xs ys)
    zipWithSumLength :: [ArgDiscount] -> [ArgSummary] -> (Int, Int)
    zipWithSumLength dcs args = go 0 0 dcs args
      where
        go !length !discount (dc:dcs) (arg:args) =
          let arg_discount = mk_arg_discount dc arg
          in go (1+length) (discount + arg_discount) dcs args
        go l d [] _ = (l,d)
        go l d _ [] = (l,d)

    res_discount'
      | LT <- arg_discounts `compareLength` arg_infos
      = res_discount   -- Over-saturated
      | otherwise
      = case cont_info of
           BoringCtxt  -> 0
           CaseCtxt    -> res_discount  -- Presumably a constructor
           ValAppCtxt  -> res_discount  -- Presumably a function
           _           -> 40 `min` res_discount
                -- ToDo: this 40 `min` res_discount doesn't seem right
                --   for DiscArgCtxt it shouldn't matter because the function will
                --       get the arg discount for any non-triv arg
                --   for RuleArgCtxt we do want to be keener to inline; but not only
                --       constructor results
                --   for RhsCtxt I suppose that exposing a data con is good in general
                --   And 40 seems very arbitrary
                --
                -- res_discount can be very large when a function returns
                -- constructors; but we only want to invoke that large discount
                -- when there's a case continuation.
                -- Otherwise we, rather arbitrarily, threshold it.  Yuk.
                -- But we want to avoid inlining large functions that return
                -- constructors into contexts that are simply "interesting"

{- Note [Interesting arguments]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
An argument is interesting if it deserves a discount for unfoldings
with a discount in that argument position.  The idea is to avoid
unfolding a function that is applied only to variables that have no
unfolding (i.e. they are probably lambda bound): f x y z There is
little point in inlining f here.

Generally, *values* (like (C a b) and (\x.e)) deserve discounts.  But
we must look through lets, eg (let x = e in C a b), because the let will
float, exposing the value, if we inline.  That makes it different to
exprIsHNF.

Before 2009 we said it was interesting if the argument had *any* structure
at all; i.e. (hasSomeUnfolding v).  But does too much inlining; see #3016.

But we don't regard (f x y) as interesting, unless f is unsaturated.
If it's saturated and f hasn't inlined, then it's probably not going
to now!

Note [Conlike is interesting]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider
        f d = ...((*) d x y)...
        ... f (df d')...
where df is con-like. Then we'd really like to inline 'f' so that the
rule for (*) (df d) can fire.  To do this
  a) we give a discount for being an argument of a class-op (eg (*) d)
  b) we say that a con-like argument (eg (df d)) is interesting
-}

interestingArg :: SimplEnv -> CoreExpr -> ArgSummary
-- See Note [Interesting arguments]
interestingArg env e =
  go env depth_limit 0 e
    where
    depth_limit = unfoldingMaxDiscountDepth . sm_uf_opts . seMode $ env

    -- n is # value args to which the expression is applied
    go :: SimplEnv -> Int -> Int -> CoreExpr -> ArgSummary
    go !_env max_depth _n !_
      | max_depth <= 0 = TrivArg
    go env depth n (Var v)
       = case substId env v of
           DoneId v'            -> go_var depth n v'
           DoneEx e _           -> go (zapSubstEnv env) depth            n e
           ContEx tvs cvs ids e -> go (setSubstEnv env tvs cvs ids) depth n e

    go _ _depth  _ (Lit l)
       | isLitRubbish l        = TrivArg -- Leads to unproductive inlining in WWRec, #20035
       | otherwise             = ValueArg
    go _ _depth   _ (Type _)          = TrivArg
    go _ _depth   _ (Coercion _)      = TrivArg
    go env depth n (App fn (Type _)) = go env depth n fn
    go env depth n e@(App _fn _arg)
      | (fn,arg_summaries,_ticks) <- mapArgsTicksVal (go env (depth-1) 0) e
      = let fn_summary = go env depth (n + length arg_summaries) fn
        in case fn_summary of
          ConArg con fn_args
            | isClassTyCon (dataConTyCon con) -> ValueArg
            | otherwise ->
              assert (null fn_args) $
              ConArg con arg_summaries
          _ -> fn_summary

    go env depth n (Tick _ a)        = go env depth n a
    go env depth n (Cast e _)        = go env depth n e
    go env depth n (Lam v e)
       | isTyVar v             = go env depth n e
       | n>0                   = NonTrivArg     -- (\x.b) e   is NonTriv
       | otherwise             = ValueArg
    go _ _depth _ (Case {})           = NonTrivArg
    go env depth n (Let b e)         = case go env' depth n e of
                                   ValueArg -> ValueArg
                                   c@ConArg{} -> c
                                   _        -> NonTrivArg
                               where
                                 env' = env `addNewInScopeBndr` b

    go_var depth n v
      | Just rhs <- maybeUnfoldingTemplate (idUnfolding v)
      , (f, arg_summaries, _ticks) <- mapArgsTicksVal (go env (depth-1) 0) rhs
      , Var f' <- varView f
      , Just con <- isDataConId_maybe f'
      , not (isClassTyCon $ dataConTyCon con)
      =
        -- pprTrace "ConArg1" (ppr $ ConArg con $ map (go env 0) args) $
        ConArg con arg_summaries

      | Just con <- isDataConId_maybe v
      = ConArg con []
      | isConLikeId v     = ValueArg   -- Experimenting with 'conlike' rather that
                                      --    data constructors here
      | idArity v > n     = ValueArg   -- Catches (eg) primops with arity but no unfolding
      | n > 0             = NonTrivArg -- Saturated or unknown call
      | conlike_unfolding = ValueArg   -- n==0; look for an interesting unfolding
                                      -- See Note [Conlike is interesting]
      | otherwise         = TrivArg    -- n==0, no useful unfolding
      where
        conlike_unfolding = isConLikeUnfolding (idUnfolding v)

        varView (Cast e _) = e
        varView (Tick _ e) = e
        varView e = e

-- | Like @collectArgs@, but maps over the arguments at the same time.
-- and also looks through casts.
mapArgsTicksVal :: (Expr b -> c) -> Expr b
                 -> (Expr b, [c], [CoreTickish])
mapArgsTicksVal fm expr
  = go expr [] []
  where
    go (App f a)  as ts
      | isValArg a = go f (fm a:as) ts
      | otherwise = go f as ts
    go (Tick t e) as ts = go e as (t:ts)
    go (Cast e _) as ts = go e as ts
    go e          as ts = (e, as, reverse ts)
