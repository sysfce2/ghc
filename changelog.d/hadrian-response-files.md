section: packaging
synopsis: Add a flag to tell Hadrian to keep response files
issues: #27184
mrs: !15906
description:
  Hadrian can now be instructed to keep response files with the new
  --keep-response-files command line flag. This is helpful when debugging a
  build failure, as it allows re-running the failing command line invocation
  without an error due to a missing response file.
