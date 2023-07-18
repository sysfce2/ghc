def normalise_nos(s):
  return re.sub(r':\d+:\d+','<line>:<no>', s)

# Check that ls-files works and returns files from the source tree.
# We just check that "hie.yaml" is there because it's top-level (dont have have to deal with
# path differences) and quite unique to GHC.

try:
  files = subprocess.check_output(['git', 'ls-files']).splitlines()
  has_ls_files = b"hie.yaml" in files
except subprocess.CalledProcessError:
  has_ls_files = False

req_ls_files = unless(has_ls_files, skip)

setTestOpts(no_deps)  # linters don't need GHC to be built

test('uniques', [extra_files(["checkUniques"])], makefile_test, ['uniques'])

test('makefiles', [ req_ls_files
                  , extra_files(["regex-linters"]) ]
                , makefile_test, ['makefiles'])

test('changelogs', [ req_ls_files
                   , extra_files(["regex-linters"]) ]
                 , makefile_test, ['changelogs'])

test('cpp', [ req_ls_files
            , extra_files(["regex-linters"]) ]
          , makefile_test, ['cpp'])

test('rts-includes', [ req_ls_files
                     , extra_files(["regex-linters"]) ]
                   , makefile_test, ['rts-includes'])

test('version-number', [ req_ls_files
                       , extra_files(["regex-linters"]) ]
                     , makefile_test, ['version-number'])

test('notes', [ req_ls_files
              , req_hadrian_deps(["lint:notes"])
              , normalise_fun(normalise_nos) ]
            , makefile_test, ['notes'])

test('whitespace', [ req_ls_files
                   , req_hadrian_deps(["lint:whitespace"]) ]
                 , makefile_test, ['whitespace'])
