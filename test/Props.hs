-- |
-- Module:     Main
-- Copyright:  (c) 2013 Ertugrul Soeylemez
-- License:    BSD3
-- Maintainer: Ertugrul Soeylemez <es@ertes.de>
--
-- Test program for the pipes-bytestring package.

module Main where

import Props.List
import Test.Framework


main :: IO ()
main = defaultMain [listProps]
