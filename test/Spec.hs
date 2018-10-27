import Test.HUnit
import Lib

main :: IO Counts
main = runTestTT tests

tests = test [
  "Finding out the square root with the binary search algorithm must" ~:
    "return NaN for invalid numbers" ~: s1,
    "return ~1 when 1.0 is given" ~: s2,
    "return ~2 when 4.0 is given" ~: s3,
    "return ~0 when 0.0 is given" ~: s4,
    "return ~4 when 16.0 is given" ~: s5,
    "return ~31.62277660169366 when 1000.0 is given" ~: s6,
    "return ~1000 when 1000000.0 is given" ~: s7,
    "work with a big number" ~: s8,
    "work with a small number" ~: s9 ]

s1 :: Test
s1 = isNaN (sqrt' (-1)) ~? "should return NaN"

s2 :: Test
s2 = sqrt' 1.0 ~== 1.0 ~? "should return 1.0" 

s3 :: Test
s3 = sqrt' 4.0 ~== 2.0 ~? "should return 2.0" 

s4 :: Test
s4 = sqrt' 0.0 ~== 0.0 ~? "should return 0.0" 

s5 :: Test
s5 = sqrt' 16.0 ~== 4.0 ~? "should return 4.0"

s6 :: Test
s6 = sqrt' 1000.0 ~== sqrt 1000.0 ~? "should return 31.622776"

s7 :: Test
s7 = sqrt' 1000000.0 ~== sqrt' 1000000.0 ~? "should return 1000.0"

s8 :: Test
s8 = sqrt' big ~== sqrt big ~? "should work with big numbers"
  where maxInt = maxBound :: Int
        decrease = flip (/) $ 100000
        big = sqrt . decrease . fromIntegral $ maxInt

s9 :: Test
s9 = sqrt' 0.0000000001 ~== sqrt 0.0000000001 ~? "should work with small numbers" 
