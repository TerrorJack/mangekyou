{-# LANGUAGE TemplateHaskell #-}

module Mangekyou.BuildInfo
  ( mangekyouLocalBuildInfo
  ) where

import Data.Binary
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Unsafe as BS
import Distribution.Simple.LocalBuildInfo
import Language.Haskell.TH.Syntax
import System.IO.Unsafe

mangekyouLocalBuildInfo :: LocalBuildInfo
{-# NOINLINE mangekyouLocalBuildInfo #-}
mangekyouLocalBuildInfo =
  $(do lbi <- runIO $ LBS.unpack <$> LBS.readFile "mangekyou.lbi.buildinfo"
       [|decode
           (LBS.fromStrict
              (unsafePerformIO
                 (BS.unsafePackAddressLen
                    $(lift (length lbi))
                    $(pure (LitE (StringPrimL lbi))))))|])
