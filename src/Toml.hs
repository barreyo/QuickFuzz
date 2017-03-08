{-# LANGUAGE TemplateHaskell, FlexibleInstances, IncoherentInstances, ConstraintKinds #-}

module TOML where

import Data.DeriveTH

import Mutation
import DeriveArbitrary
import DeriveMArbitrary
import DeriveMutation
import Strings

import Test.QuickCheck
import Text.Toml.Types
import Data.Time.Clock

import qualified Data.ByteString.Lazy.Char8 as LC8

instance Arbitrary UTCTime where
    arbitrary = return $ UTCTime 3 (secondsToDiffTime 23)

instance Arbitrary String where
   arbitrary = mgenName

$(devArbitrary ''Table)

mencode :: Table -> LC8.ByteString
mencode x = undefined
