module Props.Utils
    ( module Control.Proxy,
      module Control.Proxy.ByteString,
      module Data.Word,
      module Props.Utils,
      module Test.Framework.Providers.QuickCheck2,
      module Test.Framework.TH,
      module Test.QuickCheck
    )
    where

import Control.Monad.Writer
import Control.Proxy
import Control.Proxy.ByteString
import Data.Word
import Test.Framework.Providers.QuickCheck2
import Test.Framework.TH
import Test.QuickCheck
import Test.QuickCheck.Instances ()


equals :: (Eq a, Show a) => a -> a -> Property
equals g e =
    printTestCase ("Expected: " ++ show e) .
    printTestCase ("Actual:   " ++ show g) $
    g == e


writeProxy :: (Monoid l) => (() -> ProxyFast a' () () b (Writer l) r) -> l
writeProxy = execWriter . runProxy
