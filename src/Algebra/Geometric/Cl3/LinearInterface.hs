{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE ViewPatterns #-}


--------------------------------------------------------------------------------------------
-- |
-- Copyright   :  (C) 2018 Nathan Waivio
-- License     :  BSD3
-- Maintainer  :  Nathan Waivio <nathan.waivio@gmail.com>
-- Stability   :  Stable
-- Portability :  unportable
-- 
-- Interface functions of Cl3 types to/from Linear. 
-- 
-------------------------------------------------------------------


module Algebra.Geometric.Cl3.LinearInterface
(
 toLinearV3, 
 fromLinearV3, 
 toLinearQuaternion,
 fromLinearQuaternion,
 toLinearM22,
 fromLinearM22
) where

import safe Algebra.Geometric.Cl3 (Cl3(..), toV3, toH, toAPS, reduce)
import qualified Linear (V3(..), Quaternion(..), V2(..), M22)
import safe Data.Complex (Complex(..))

-- | 'toLinearV3' Convert a Cl3 V3 to a Linear V3
toLinearV3 :: Cl3 -> Linear.V3 Double
toLinearV3 (toV3 -> (V3 a1 a2 a3)) = Linear.V3 a1 a2 a3
toLinearV3 _ = error "Pattern Matching failure of toLinearV3 for the toV3/V3 View Pattern"

-- | 'fromLinearV3' Convert from a Linear V3 to a Cl3 V3
fromLinearV3 :: Real a => Linear.V3 a -> Cl3
fromLinearV3 (Linear.V3 (toRational -> a1) (toRational -> a2) (toRational -> a3)) = V3 (fromRational a1) (fromRational a2) (fromRational a3)

-- | 'toLinearQuaternion' Convert a Cl3 H to a Linear Quaternion
toLinearQuaternion :: Cl3 -> Linear.Quaternion Double
toLinearQuaternion (toH -> (H a0 a32 a31 a12)) = Linear.Quaternion a0 (Linear.V3 a32 a31 a12)
toLinearQuaternion _ = error "Pattern Matching failure of toLinearQuaternion for the toH/H View Pattern"

-- | 'fromLinearQuaternion' Convert from a Linear Quaternion to a Cl3 H
fromLinearQuaternion :: Real a => Linear.Quaternion a -> Cl3
fromLinearQuaternion (Linear.Quaternion (toRational -> a0) (Linear.V3 (toRational -> a32) (toRational -> a31) (toRational -> a12))) = 
  H (fromRational a0) (fromRational a32) (fromRational a31) (fromRational a12)

-- | 'toLinearM22' Convert a Cl3 Cliffor to a Linear M22
toLinearM22 :: Cl3 -> Linear.M22 (Complex Double)
toLinearM22 (toAPS -> APS a0 a1 a2 a3 a23 a31 a12 a123) = 
  let a = (a0+a3) :+ (a123+a12)
      b = (a1+a31) :+ (a23-a2)
      c = (a1-a31) :+ (a23+a2)
      d = (a0-a3) :+ (a123-a12)
  in Linear.V2 (Linear.V2 a b) (Linear.V2 c d)
toLinearM22 _ = error "Pattern Matching failure of toLinearM22 for the toAPS/APS View Pattern"

-- | 'fromLinearM22' Convert from a Linear M22 to a Cl3 Cliffor
fromLinearM22 :: Real a => Linear.M22 (Complex a) -> Cl3
fromLinearM22 (Linear.V2 (Linear.V2 (a :+ ai) (b :+ bi)) (Linear.V2 (c :+ ci) (d :+ di))) =
  let a' = fromRational (toRational a) :: Double
      ai' = fromRational (toRational ai) :: Double
      b' = fromRational (toRational b) :: Double
      bi' = fromRational (toRational bi) :: Double
      c' = fromRational (toRational c) :: Double
      ci' = fromRational (toRational ci) :: Double
      d' = fromRational (toRational d) :: Double
      di' = fromRational (toRational di) :: Double
      a0 = (a' + d') / 2
      a1 = (b' + c') / 2
      a2 = (ci' - bi') / 2
      a3 = (a' - d') / 2
      a23 = (ci' + bi') / 2
      a31 = (b' - c') / 2
      a12 = (ai' - di') / 2
      a123 = (ai' + di') / 2
  in reduce (APS a0 a1 a2 a3 a23 a31 a12 a123)

