{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Mangekyou.LocalBuildInfo
  ( mangekyouSetSessionDynFlags
  , mangekyouLocalBuildInfo
  , dynFlagsWithLocalBuildInfo
  ) where

import Data.Binary
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Unsafe as BS
import Data.Functor
import Distribution.Simple.Compiler
import Distribution.Simple.LocalBuildInfo
import DynFlags
import GHC
import Language.Haskell.TH.Syntax
import System.IO.Unsafe

mangekyouSetSessionDynFlags :: GhcMonad m => m ()
mangekyouSetSessionDynFlags = do
  dflags <- getSessionDynFlags
  void $
    setSessionDynFlags $
    dynFlagsWithLocalBuildInfo mangekyouLocalBuildInfo dflags

mangekyouLocalBuildInfo :: LocalBuildInfo
{-# NOINLINE mangekyouLocalBuildInfo #-}
mangekyouLocalBuildInfo =
  $(do lbi <- runIO $ LBS.unpack <$> LBS.readFile "mangekyou.lbi.buildinfo"
       [|unsafePerformIO $ do
           bs <-
             BS.unsafePackAddressLen
               $(lift (length lbi))
               $(pure (LitE (StringPrimL lbi)))
           pure $ decode $ LBS.fromStrict bs|])

dynFlagsWithLocalBuildInfo :: LocalBuildInfo -> DynFlags -> DynFlags
dynFlagsWithLocalBuildInfo LocalBuildInfo {..} dflags =
  dflags
  { packageDBFlags =
      [ PackageDB $
      case pkgdb of
        GlobalPackageDB -> GlobalPkgConf
        UserPackageDB -> UserPkgConf
        SpecificPackageDB p -> PkgConfFile p
      | pkgdb <- reverse withPackageDB
      ] ++
      [ClearPackageDBs]
  }
