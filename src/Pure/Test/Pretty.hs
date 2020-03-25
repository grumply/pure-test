{-# LANGUAGE FlexibleInstances, UndecidableInstances, ExistentialQuantification #-}
module Pure.Test.Pretty where

import Pure.Spacetime
import Text.Printf

class Pretty a where
  pretty :: a -> String

instance Pretty [Char] where
  pretty = id

data Showable = forall s. Show s => Showable s

-- for use with deriving via:
-- 
-- > data X = X Int deriving Pretty via Showable

instance Pretty Showable where
  pretty (Showable s) = show s

instance Pretty BytesPerSecond where
    pretty (BytesPerSecond t)
        | isNaN t   = "0 B/s"
        | t < 2^10  = printf "%.0f B/s"  t
        | t < 2^20  = printf "%.1f KB/s" (t / 2^10)
        | t < 2^30  = printf "%.1f MB/s" (t / 2^20)
        | t < 2^40  = printf "%.1f GB/s" (t / 2^30)
        | otherwise = printf "%.1f TB/s" (t / 2^40)

instance Pretty Seconds where
    pretty (Seconds_ d)
        | d < 1e-15 = printf     "%.0fas" (d * 1e18)
        | d < 1e-12 = printf     "%.0ffs" (d * 1e15)
        | d < 1e-9  = printf     "%.0fps" (d * 1e12)
        | d < 1e-6  = printf     "%.3fns" (d * 1e9)
        | d < 1e-3  = printf     "%.3fμs" (d * 1e6)
        | d < 1     = printf     "%.3fms" (d * 1e3)
        | d < 60    = printf     "%.3fs"   d
        | d < 60^2  = printf "%.0fm %ds"  (d / 60)   (roundi d `mod` 60)
        | otherwise = printf "%.0fh %dm"  (d / 60^2) (roundi d `mod` 60^2)
      where
        roundi :: Double -> Int
        roundi = round

instance Pretty SpaceInBytes where
    pretty (SpaceInBytes b)
        | b < 1e-15 = printf "%.2f aB"  (b * 1e18)
        | b < 1e-12 = printf "%.2f fB"  (b * 1e15)
        | b < 1e-9  = printf "%.2f pB"  (b * 1e12)
        | b < 1e-6  = printf "%.2f nB"  (b * 1e9)
        | b < 1e-3  = printf "%.2f μB"  (b * 1e6)
        | b < 1     = printf "%.2f mB"  (b * 1e3)
        | b < 1e3   = printf "%.0f B"    b
        | b < 1e6   = printf "%.2f KiB" (b / 1e3)
        | b < 1e9   = printf "%.2f MiB" (b / 1e6)
        | b < 1e12  = printf "%.2f GiB" (b / 1e9)
        | b < 1e15  = printf "%.2f TiB" (b / 1e12)
        | b < 1e18  = printf "%.2f PiB" (b / 1e15)
        | b < 1e21  = printf "%.2f EiB" (b / 1e18)
        | b < 1e24  = printf "%.2f ZiB" (b / 1e21)
        | otherwise = printf "%.2f YiB" (b / 1e24)

instance  Pretty Hertz where
    pretty (Hertz_ r) = printf "%.2f/s" r

instance Pretty Percent where
    pretty (Percent_ r)
      | isNaN r   = "0%"
      | otherwise = printf "%.2f%%" (100 * r)

instance Pretty Multiplier where
    pretty (Multiplier_ f) = printf "%.2fx" f

instance Pretty Count where
  pretty (Count_ c) = printf "%.0f" c

