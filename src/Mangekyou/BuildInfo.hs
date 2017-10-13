{-# LANGUAGE TemplateHaskell #-}

module Mangekyou.BuildInfo where

import Data.Binary
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Unsafe as BS
import Distribution.Simple.LocalBuildInfo
import Language.Haskell.TH
import System.IO.Unsafe

mangekyouLocalBuildInfo :: LocalBuildInfo
{-# NOINLINE mangekyouLocalBuildInfo #-}
mangekyouLocalBuildInfo =
  $(do lbi <- runIO $ LBS.unpack <$> LBS.readFile "mangekyou.lbi.buildinfo"
       [|decode
           (LBS.fromStrict
              (unsafePerformIO
                 (BS.unsafePackAddressLen
                    $(pure (LitE (IntegerL (fromIntegral (length lbi)))))
                    $(pure (LitE (StringPrimL lbi))))))|])
