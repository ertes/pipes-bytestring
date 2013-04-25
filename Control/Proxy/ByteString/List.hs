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
      dropD,
      dropWhileD,
      takeD,
      takeWhileD
    )
    where

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as Bl
import Control.Proxy hiding (dropD, dropWhileD, takeWhileD)
import Data.ByteString (ByteString)
import Data.Word


-- | Equivalent to 'B.drop'.

dropD ::
    (Monad m, Proxy p)
    => Int  -- ^ Number of initial bytes to drop.
    -> () -> Pipe p ByteString ByteString m r
dropD n = runIdentityK (loop n >=> idT)
    where
    loop n =
        request >=> \bs ->
            let len = B.length bs in
            if len < n
              then loop (n - len) ()
              else respond (B.drop n bs)


-- | Equivalent to 'B.dropWhile'.

dropWhileD ::
    (Monad m, Proxy p)
    => (Word8 -> Bool)  -- ^ Drop bytes while this predicate is true.
    -> () -> Pipe p ByteString ByteString m r
dropWhileD p = runIdentityK (loop >=> idT)
    where
    loop =
        request >=> \bs ->
            let sfx = B.dropWhile p bs in
            if B.null sfx
              then loop ()
              else respond sfx


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


-- | Equivalent to 'B.takeWhile'.

takeWhileD ::
    (Monad m, Proxy p)
    => (Word8 -> Bool)  -- ^ Take bytes while this predicate is true.
    -> () -> Pipe p ByteString ByteString m ()
takeWhileD p = runIdentityK loop
    where
    loop =
        request >=> \bs ->
            let (pfx, sfx) = B.span p bs in
            if B.null sfx
              then respond pfx >>= loop
              else respond pfx


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
