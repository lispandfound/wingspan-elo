{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module PackageInfo_semialign (
    name,
    version,
    synopsis,
    copyright,
    homepage,
  ) where

import Data.Version (Version(..))
import Prelude

name :: String
name = "semialign"
version :: Version
version = Version [1,3,1] []

synopsis :: String
synopsis = "Align and Zip type-classes from the common Semialign ancestor."
copyright :: String
copyright = ""
homepage :: String
homepage = "https://github.com/haskellari/these"
