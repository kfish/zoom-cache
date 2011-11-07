-- | The minimum and maximum positive, finite floats.

module Data.ZoomCache.Numeric.FloatMinMax
( floatMin
, floatMaxDenorm
, floatMinNorm
, floatMax
)
where

import Control.Applicative
import Data.Function

-- | The minimum positive, denormalized float.
floatMin :: RealFloat a => a
floatMin = fix $ do d     <- floatDigits
                    (e,_) <- floatRange
                    pure $ encodeFloat 1 (e-d)
{-# INLINE floatMin #-}

-- | The maximum denormalized float.
floatMaxDenorm :: RealFloat a => a
floatMaxDenorm = floatMinNorm - floatMin
{-# INLINE floatMaxDenorm #-}

-- | The minimum positive, normalized float.
floatMinNorm :: RealFloat a => a
floatMinNorm = fix $ do (e,_) <- floatRange
                        pure $ encodeFloat 1 (e-1)
{-# INLINE floatMinNorm #-}

-- | The maximum finite float.
floatMax :: RealFloat a => a
floatMax = fix $ do r     <- floatRadix
                    d     <- floatDigits
                    (_,e) <- floatRange
                    pure $ encodeFloat (r^d-1) (e-d)
{-# INLINE floatMax #-}
