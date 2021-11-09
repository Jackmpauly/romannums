-- Roman Numeral Data Types:

-- I = 1
-- V = 5
-- X = 10
-- L = 50
-- C = 100
-- D = 500
-- M = 1000

-- conversions = [(1000,"M"), (900,"CM"), (500,"D"), (400,"CD"), (100,"C"), (90,"XC"), (50,"L"), (40,"XL"), (10,"X"), (5,"V"), (4,"IV"), (1,"I")]

data RN = O | I RN | V RN | X RN | L RN | C RN | D RN | M RN
  deriving (Eq,Show) -- for equality and printing


rnInt :: RN -> Int
rnInt O = 0;
rnInt (M a) = 1000 + rnInt a
rnInt (C (M a)) = 900 + rnInt a
rnInt (D a) = 500 + rnInt a
rnInt (C (D a)) = 400 + rnInt a
rnInt (C a) = 100 + rnInt a
rnInt (X (C a)) = 90 + rnInt a
rnInt (L a) = 50 + rnInt a
rnInt (X (L a)) = 40 + rnInt a
rnInt (X a) = 10 + rnInt a
rnInt (I (X a)) = 9 + rnInt a
rnInt (V a) = 5 + rnInt a
rnInt (I (V a)) = 4 + rnInt a
rnInt (I a) = 1 + rnInt a

intRn :: Int -> RN
intRn 0 = O
intRn n
    | n `div` 1000 > 0 = M (intRn (n-1000) )
    | n `div` 900 > 0 = C (M (intRn (n-900)) )
    | n `div` 500 > 0 = D (intRn (n-500) )
    | n `div` 400 > 0 = C (D (intRn (n-400)) )
    | n `div` 100 > 0 = C (intRn (n-100) )
    | n `div` 90 > 0 = X (C (intRn (n-90)) )
    | n `div` 50 > 0 = L (intRn (n-50) )
    | n `div` 40 > 0 = X (L (intRn (n-40)) )
    | n `div` 10 > 0 = X (intRn (n-10) )
    | n `div` 9 > 0 = I (X (intRn (n-9)) )
    | n `div` 5 > 0 = V (intRn (n-5) )
    | n `div` 4 > 0 = I (V (intRn (n-4)) )
    | n `div` 1 > 0 = I (intRn (n-1) )
    | otherwise = O

rnString :: RN -> String
rnString O = ""
rnString (M a) = "M" ++ rnString a
rnString (C (M a)) = "CM" ++ rnString a
rnString (D a) = "D" ++ rnString a
rnString (C (D a)) = "CD" ++ rnString a
rnString (C a) = "C" ++ rnString a
rnString (X (C a)) = "XC" ++ rnString a
rnString (L a) = "L" ++ rnString a
rnString (X (L a)) = "XL" ++ rnString a
rnString (X a) = "X" ++ rnString a
rnString (I (X a)) = "IX" ++ rnString a
rnString (V a) = "V" ++ rnString a
rnString (I (V a)) = "IV" ++ rnString a
rnString (I a) = "I" ++ rnString a

convert :: Int -> String
convert n = rnString (intRn n)


main = do

    print "ROMAN NUMERALS"
    let rn = X (V (I (I O))) -- = 17
    let rn14 = X (I (V O)) -- = 14
    let i = 1100
    print $ rnInt rn14
    print $ rnString rn14
    print $ intRn 9
    print $ convert 2017
    print $ convert 217
    print $ convert 994