{-# OPTIONS_GHC -fno-warn-missing-fields #-}
-- |
-- Module      : Data.UUID.Quasi
-- Copyright   : (c) 2011 Lars Petersen
--
-- License     : BSD-style
--
-- Maintainer  : info@lars-petersen.net
-- Stability   : experimental
-- Portability : portable
-- 
-- 
-- This library supplies quasiquotation of 'U.UUID's. You should use this in
-- case you want to hardcode 'U.UUID's in your sourcecode with compile-time
-- checking for syntax errors.

module Data.UUID.Quasi (uuid) where

import qualified Data.UUID as U

import Language.Haskell.TH.Lib
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax

uuidWords :: String -> Q [Lit]
uuidWords uuidStr =
    case U.fromString uuidStr of
      Nothing -> fail "not a valid UUID"
      Just u ->
          case U.toWords u of
            (w1,w2,w3,w4) ->
                return $ map (IntegerL . toInteger) [w1,w2,w3,w4]

expUUID :: String -> Q Exp
expUUID uuidStr = do
  wds <- uuidWords uuidStr
  let litNums = map LitE wds
  fromExp <- varE 'U.fromWords
  return $ foldl AppE fromExp litNums

patUUID :: String -> Q Pat
patUUID uuidStr = do
  wds <- uuidWords uuidStr
  let patNums = map LitP wds
      patTup  = TupP patNums
  toExp <- varE 'U.toWords
  return $ ViewP toExp patTup

{- | The quasiquoter for expressions and patterns of 'U.UUID'. Make sure to enable '-XQuasiQuotes'.

> > let a = [uuid|550e8400-e29b-41d4-a716-446655440000|]
> > :type a
> a :: UUID
> > case a of { [uuid|550e8400-e29b-41d4-a716-446655440000|] -> True; _ -> False; }
> True

Pattern matching requires '-XViewPatterns'.
-}
uuid :: QuasiQuoter
uuid  = QuasiQuoter
          { quoteExp = expUUID
          , quotePat = patUUID
          }
