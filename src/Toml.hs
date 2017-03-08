{-# LANGUAGE TemplateHaskell, FlexibleInstances, IncoherentInstances, ConstraintKinds #-}

module TOML where

import           Data.DeriveTH

import           Mutation
import           DeriveArbitrary
import           DeriveMArbitrary
import           DeriveMutation
import           Strings
import           Vector

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
      vs <- arbitrary
      vi <- arbitrary
      vf <- arbitrary
      vb <- arbitrary
      vt <- arbitrary
      n <- elements [VString vs, VInteger vi, VFloat vf, VBoolean vb, VDatetime vt]
      return n

instance Arbitrary VTArray where
    arbitrary = do
        a <- arbitrary :: Vector Table
        return a

instance Arbitrary VArray where
    arbitrary = arbitrary :: Vector Node

instance Arbitrary Table where
    arbitrary = do
        x <- listOf arbitrary
        y <- listOf arbitrary
        return $ M.fromList (zip x y)

mencode :: Table -> LC8.ByteString
mencode x = undefined
