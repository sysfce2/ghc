module Settings.TargetDirectory (
    targetDirectory, targetPath, pkgHaddockFile
    ) where

import Base
import Stage
import Package
import Settings.User

-- User can override the default target directory settings given below
targetDirectory :: Stage -> Package -> FilePath
targetDirectory = userTargetDirectory

-- Path to the target directory from GHC source root
targetPath :: Stage -> Package -> FilePath
targetPath stage pkg = pkgPath pkg -/- targetDirectory stage pkg

-- Relative path to a package haddock file, e.g.:
-- "libraries/array/dist-install/doc/html/array/array.haddock"
pkgHaddockFile :: Package -> FilePath
pkgHaddockFile pkg @ (Package name _) =
    targetPath Stage1 pkg -/- "doc/html" -/- name -/- name <.> "haddock"
