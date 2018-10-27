module Lib
    ( (~==),
      epsilon,
      sqrt'
    ) where

(~==) :: Double -> Double -> Bool
d1 ~== d2 = (d2 - epsilon) <= d1 && d1 <= (d2 + epsilon)

epsilon :: Double
epsilon = 0.000000001

sqrt' :: Double -> Double
sqrt' d = if (d < 0)
          then nan
          else if (d < 1)
          then inverse . sqrt' . inverse $ d
          else bSearch 0 d
          where inverse n = 1 / n
                nan = 0 / 0
                bSearch :: Double -> Double -> Double
                bSearch min max = let mean = (min + max) / 2
                                      res = mean * mean
                                  in if (res ~== d)
                                     then mean
                                     else if (res > d + epsilon)
                                     then bSearch min mean
                                     else bSearch mean max
