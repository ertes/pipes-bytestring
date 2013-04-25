-- |
-- Module:     Control.Proxy.ByteString.List
-- Copyright:  (c) 2013 Ertugrul Soeylemez
-- License:    BSD3
-- Maintainer: Ertugrul Soeylemez <es@ertes.de>

module Control.Proxy.ByteString.List
    ( -- * Basic operations
      fromLazyS,
      unfoldrS,
      unpackD,

      -- * Substreams
      takeD
    )
    where

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as Bl
import Control.Proxy
import Data.ByteString (ByteString)
import Data.Word


-- | Turn the given lazy 'ByteString' into a stream of its strict
-- chunks.

fromLazyS :: (Monad m, Proxy p) => Bl.ByteString -> () -> Producer p ByteString m ()
fromLazyS = fromListS . Bl.toChunks


-- | Equivalent of 'B.take'.

takeD ::
    (Monad m, Proxy p)
    => Int  -- ^ Number of bytes to take.
    -> () -> Pipe p ByteString ByteString m ()
takeD = runIdentityK . loop
    where
    loop n =
        request >=> \bs ->
            let len = B.length bs in
            if len < n
              then respond bs >>= loop (n - len)
              else respond (B.take n bs)


-- | Equivalent of 'B.unfoldr'.  Generate a 'ByteString' stream using
-- the given generator function.  Notice that the first argument is only
-- the chunk size, not the maximum size.

unfoldrS ::
    (Monad m, Proxy p)
    => Int                      -- ^ Chunk size.
    -> (a -> Maybe (Word8, a))  -- ^ Generator function.
    -> a                        -- ^ Seed.
    -> () -> Producer p ByteString m ()
unfoldrS n f = runIdentityK . loop
    where
    loop x =
        let (bs, my) = B.unfoldrN n f x in
        const (respond bs) >=>
        maybe return loop my


-- | Unpack a stream of 'ByteString's into individual bytes.

unpackD :: (Monad m, Proxy p) => () -> Pipe p ByteString Word8 m r
unpackD = runIdentityK (foreverK $ request >=> mapM_ respond . B.unpack)
