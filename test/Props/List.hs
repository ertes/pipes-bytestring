module Props.List where

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as Bl
import Props.Utils


prop_dropD n bs =
    Bl.fromChunks (writeProxy $ fromLazyS bs >-> dropD n >-> toListD) `equals`
    Bl.drop (fromIntegral n) bs

prop_dropWhileD (Blind p) bs =
    Bl.fromChunks (writeProxy $ fromLazyS bs >-> dropWhileD p >-> toListD) `equals`
    Bl.dropWhile p bs

prop_fromLazyS bs =
    writeProxy (fromLazyS bs >-> toListD) `equals`
    Bl.toChunks bs

prop_takeD n bs =
    Bl.fromChunks (writeProxy $ fromLazyS bs >-> takeD n >-> toListD) `equals`
    Bl.take (fromIntegral n) bs

prop_takeWhileD (Blind p) bs =
    Bl.fromChunks (writeProxy $ fromLazyS bs >-> takeWhileD p >-> toListD) `equals`
    Bl.takeWhile p bs

prop_unfoldrS (Blind f) x =
    forAll (choose (1, 16384)) $ \m ->
    forAll (choose (1, 4096)) $ \n ->
        B.concat (writeProxy $ unfoldrS n f x >-> takeD m >-> toListD) `equals`
        fst (B.unfoldrN m f x)
        where _ = x :: Int

prop_unpackD bs =
    writeProxy (fromLazyS bs >-> unpackD >-> toListD) `equals`
    Bl.unpack bs


listProps = $(testGroupGenerator)
