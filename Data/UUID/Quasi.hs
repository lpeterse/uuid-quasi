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
-- This library supplies quasiquotation of UUIDs. You should use this in
-- case you want to hardcode UUIDs in your sourcecode with compile-time
-- checking for syntax errors.

module Data.UUID.Quasi (uuid) where

import Data.UUID

import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax

import Data.Char
import Data.List hiding (null)
import Data.List.Split

quoteExpr      :: (Name -> [Lit] -> a) -> String -> Q a
quoteExpr con x |  length x == 32
                && all isHexDigit x
                =  t [a,b,c,d]
                |  length x == 36
                && elemIndices '-' x == [8,13,18,23]
                && all isHexDigit (filter (/='-') x)
                =  t [e, f++g, h++(take 4 i), drop 4 i]
                |  otherwise 
                =  fail "a UUID must consist of 32 hexdigits optionally interspersed by 4 '-'"
                where
                  [a,b,c,d]           = splitPlaces [8 :: Int,8,8,8]            x
                  [e,_,f,_,g,_,h,_,i] = splitPlaces [8 :: Int,1,4,1,4,1,4,1,12] x
                  z                   = IntegerL . read . ('0':) . ('x':)
                  t                   = return . con (mkName "UUID") . map z

-- | The quasiquoter for expressions and patterns of 'UUID'. Make sure to enable '-XQuasiQuotes'.
-- > > let a = [uuid|550e8400-e29b-41d4-a716-446655440000|]
-- > > case a of { [uuid|550e8400-e29b-41d4-a716-446655440000|] -> True; _ -> False; }
-- > True
uuid :: QuasiQuoter
uuid  = QuasiQuoter 
          (quoteExpr $ \x-> foldl AppE (ConE x) . map LitE)  
          (quoteExpr $ \x-> ConP x              . map LitP)
          (const $ fail "undefined QuasiQuoter for Type")
          (const $ fail "undefined QuasiQuoter for Declaration")
