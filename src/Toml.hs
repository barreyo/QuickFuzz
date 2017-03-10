{-# LANGUAGE TemplateHaskell, FlexibleInstances, IncoherentInstances, ConstraintKinds, OverloadedStrings #-}

module TOML where

import           Data.DeriveTH

import           Mutation
import           DeriveArbitrary
import           DeriveMArbitrary
import           DeriveMutation
import           Strings
import           Vector

import Text.Toml.Types
import Text.PrettyPrint.HughesPJ

import qualified Data.HashMap.Strict as M
import qualified Data.Vector         as V
import           Data.Time.Clock     (UTCTime)
import           Data.Time.Format    (formatTime, defaultTimeLocale)
import qualified Data.Text           as T

import           Test.QuickCheck
import           Text.Toml.Types

import           Time

import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as M
import           Data.Text           (Text)
import qualified Data.Text           as T

import qualified Data.ByteString.Lazy.Char8 as LC8

instance Arbitrary String where
    arbitrary = mgenName

instance Arbitrary Node where
    arbitrary = do
      vs  <- arbitrary
      vi  <- arbitrary
      vf  <- arbitrary
      vb  <- arbitrary
      vt  <- arbitrary
      vta <- arbitrary
      let va = V.singleton $ VInteger vi
      n   <- elements [VString vs,  VInteger vi,  VFloat vf, VArray va,
                       VBoolean vb, VDatetime vt, VTArray vta]
      return n

instance Arbitrary Table where
    arbitrary = do
        x <- listOf arbitrary
        y <- listOf arbitrary
        return $ M.fromList (zip x y)

mencode :: Table -> LC8.ByteString
mencode x = LC8.pack $ render $ ppTable x

-- ----------------------------------------------------------------------------
-- Pretty Print TOML - TODO: Pull request open, remove once merged.
-- ----------------------------------------------------------------------------

ppNode :: Node -> Doc
ppNode n = case n of
    (VTable v)    -> ppTable v
    (VTArray v)   -> ppTArray v ""
    (VString v)   -> ppTomlString v
    (VInteger v)  -> ppInteger $ fromIntegral v
    (VFloat v)    -> ppFloat v
    (VBoolean v)  -> ppBoolean v
    (VDatetime v) -> ppDateTime v
    (VArray v)    -> ppArray v

ppTomlString :: T.Text -> Doc
ppTomlString str = doubleQuotes $ hcat $ map ppChar (T.unpack str)
    where ppChar '\\' = text "\\\\"
          ppChar '\"' = text "\\\""
          ppChar c    = char c

ppDateTime :: UTCTime -> Doc
ppDateTime t = hcat $ map ppDate (show f_date)
    where f_date      = formatTime defaultTimeLocale "%FT%TZ" t
          ppDate '\"' = text ""
          ppDate c    = char c

ppInteger :: Integer -> Doc
ppInteger = integer

ppFloat :: Double -> Doc
ppFloat = double

ppBoolean :: Bool -> Doc
ppBoolean True  = text "true"
ppBoolean False = text "false"

ppArray :: V.Vector Node -> Doc
ppArray va = brackets $ fsep $ punctuate comma $ map ppNode (V.toList va)

ppTable :: Table -> Doc
ppTable tb | M.null tb = empty
           | otherwise = findTTitle (M.toList tb) True [text ""]

findTTitle :: [(T.Text, Node)] -> Bool -> [Doc] -> Doc
findTTitle []                    True     ti = brackets $ hcat ti
findTTitle [(t, VTArray v)]      True  _  = ppTArray v t
findTTitle ((t, VTArray v) : xs) True  ti = ppTArray v t $$ findTTitle xs True ti
findTTitle [(t, VTArray v)]      False ti = ppTArray v (T.pack (render (hcat ti) ++ "." ++ T.unpack t))
findTTitle ((t, VTArray v) : xs) False ti = ppTArray v (T.pack (render (hcat ti) ++ "." ++ T.unpack t)) $$ findTTitle xs False (ti ++ [text "YPPP"])
findTTitle [(t, VTable  v)]      _     ti = findTTitle (M.toList v) True $ ti ++ [text $ T.unpack t]
findTTitle ((t, VTable  v) : xs) b     ti = findTTitle (M.toList v) True (ti ++ [text $ T.unpack t]) $$ findTTitle xs b ti
findTTitle [v]                   False _  = vcat (tableToList [v])
findTTitle (v:xs)                False ti = vcat (tableToList [v]) $$ findTTitle xs False ti 
findTTitle v                     _    [_] = vcat (tableToList v)
findTTitle v                     _     ti = brackets (hcat $ punctuate (char '.') (tail ti)) $$ vcat (tableToList v)

tableToList :: [(T.Text, Node)] -> [Doc]
tableToList = map (fsep . f)
    where f (x, y) = punctuate (space <> equals) [text $ T.unpack x, ppNode y]

ppTArray :: V.Vector Table -> T.Text -> Doc
ppTArray v t | V.toList v == [] = pt
             | otherwise = vcat $ map ((\ x -> dBrackets pt $$ findTTitle x False [pt]) . M.toList) (V.toList v)
    where pt              = text $ T.unpack t
          dBrackets x = brackets $ brackets x

-- ----------------------------------------------------------------------------
