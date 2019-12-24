{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}

module Properties.FoundationDB.Versionstamp.Internal where

import FoundationDB.Layer.Tuple.Internal
import FoundationDB.Versionstamp.Internal

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (forAll)
import Test.QuickCheck.Arbitrary (Arbitrary(..), genericShrink)
import Test.QuickCheck.Gen (oneof)

instance Arbitrary TransactionVersionstamp where
  arbitrary = TransactionVersionstamp <$> arbitrary <*> arbitrary

instance Arbitrary (Versionstamp 'Complete) where
  arbitrary = CompleteVersionstamp <$> arbitrary <*> arbitrary

instance Arbitrary (Versionstamp 'Incomplete) where
  arbitrary = IncompleteVersionstamp <$> arbitrary

versionstampProps :: SpecWith ()
versionstampProps = prop "Versionstamp Ord instance matches tuple layer serialization" $
  forAll arbitrary $ \(vs1,vs2) ->
    compare vs1 vs2 == compare (encode vs1) (encode vs2)

    where encode x = encodeTupleElems [CompleteVS x]
