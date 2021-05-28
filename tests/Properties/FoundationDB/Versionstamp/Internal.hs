{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Properties.FoundationDB.Versionstamp.Internal where

import FoundationDB.Layer.Tuple.Internal
import FoundationDB.Versionstamp.Internal as VS

import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (forAll)
import Test.QuickCheck.Arbitrary (Arbitrary(..))

instance Arbitrary TransactionVersionstamp where
  arbitrary = TransactionVersionstamp <$> arbitrary <*> arbitrary

instance Arbitrary (Versionstamp 'Complete) where
  arbitrary = CompleteVersionstamp <$> arbitrary <*> arbitrary

instance Arbitrary (Versionstamp 'Incomplete) where
  arbitrary = IncompleteVersionstamp <$> arbitrary

versionstampProps :: SpecWith ()
versionstampProps = do
  prop "Complete versionstamp encode/decode" $
    forAll arbitrary $ \(vs :: Versionstamp 'Complete) ->
      Just vs == VS.decodeVersionstamp (VS.encodeVersionstamp vs)
  prop "Versionstamp Ord instance matches tuple layer serialization" $
    forAll arbitrary $ \(vs1,vs2) ->
      compare vs1 vs2 == compare (encode vs1) (encode vs2)

      where encode x = encodeTupleElems [CompleteVS x]
