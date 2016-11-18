module Main where

import Test.Tasty (defaultMain, testGroup)

--
import Data.NineP

--
import qualified NineP.Tests

-- got this idea from
--  https://jaspervdj.be/posts/2015-03-13-practical-testing-in-haskell.html
main :: IO ()
main = defaultMain $ testGroup "Tests" [NineP.Tests.tests]
