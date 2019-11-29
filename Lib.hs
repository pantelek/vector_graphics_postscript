-- Rafał Trypus
-- rt386444

module Lib where

import Mon

-- typ R objaśniony w tekście poniżej
type R = Rational
type R2 = (R,R)

data Vec = V {vecX :: R, vecY :: R}   -- wektor 2D
data Point = P {pointX :: R, pointY :: R}-- punkt 2D

instance Eq Vec where
    V x y == V x' y' = x == x' && y == y'

instance Eq Point where
    P x y == P x' y' = x == x' && y == y'

instance Show Vec where
    show (V x y) = "wektor " ++ show x ++ ", " ++ show y

instance Show Point where
    show (P x y) = "punkt " ++ show x ++ ", " ++ show y

point :: R2 -> Point
point r = P (fst r) (snd r)

vec :: R2 -> Vec
vec r = V (fst r) (snd r)

instance Mon Vec where
    m1 = vec (0, 0)
    v1 >< v2 = vec (vecX v1 + vecX v2, vecY v1 + vecY v2)

data Picture = Pic {segments :: [(Point, Point)]}

-- odcinek pomiędzy punktami o podanych współrzędnych
line :: (R,R) -> (R,R) -> Picture
line x y = Pic [(point x, point y)]

pLine :: Point -> Point -> Picture
pLine x y = Pic [(x, y)]

-- prostokąt o podanej szerokości i wysokości zaczepiony w (0,0)
rectangle :: R -> R -> Picture
rectangle x y = Pic [(P 0 0, P x 0),
                     (P x 0, P x y),
                     (P x y, P 0 y),
                     (P 0 y, P 0 0)]

-- suma (nałożenie) dwóch rysunków
(&) :: Picture -> Picture -> Picture
p1 & p2 = Pic (segments p1 ++ segments p2)

type IntLine = ((Int,Int), (Int,Int))
type IntRendering = [IntLine]

-- Obrazowanie przy danym współczynniku powiększenia
-- z zaokrągleniem do najbliższych wartości całkowitych
renderScaled :: Int -> Picture -> IntRendering
renderScaled n p = map f (segments p) where
    n' = toRational n
    f (p1, p2) = ((round (pointX p1 * n'), round (pointY p1 * n')),
                  (round (pointX p2 * n'), round (pointY p2 * n')))

data Transform = Tr {phases :: [(R, R, R)]}

-- przesunięcie o wektor
translate :: Vec -> Transform
translate v = Tr [(vecX v, vecY v, 0)]

-- obrót wokół punktu (0,0) przeciwnie do ruchu wskazówek zegara
-- jednostki mozna sobie wybrać
rotate :: R -> Transform
rotate x = Tr [(0, 0, x)]

fullCircle :: R -- wartość odpowiadająca 1 pełnemu obrotowi (360 stopni)
fullCircle = 360

instance Mon Transform where
    m1 = Tr [(0, 0, 0)]
    (Tr []) >< (Tr l) = Tr l
    (Tr l) >< (Tr []) = Tr l  
    (Tr l1) >< (Tr l2) = Tr $ init l1 ++ (join (last l1) (head l2)) ++ tail l2 where
        join (0, 0, x) (0, 0, y)   = [(0, 0, x + y)]
        join (x, y, 0) (x', y', 0) = [(x + x', y + y', 0)]
        join el1 el2               = [el1, el2]


mysin :: R -> R
mysin x
    | x > 360 = mysin $ x - 360
    | x < 0 = mysin $ x + 360
    | x > 180 = -(mysin $ x - 180)
    | otherwise = (4 * x * (180 - x)) / (40500 - x * (180 - x))

mycos :: R -> R
mycos x = mysin (90 + x) 

myrotate :: R2 -> R -> R2
myrotate (x, y) r = (x * (mycos r) - y * (mysin r), x * (mysin r) + y * (mycos r))

trpoint :: Transform -> Point -> Point
trpoint t p = foldl f p (phases t) where
    f p1 (x, y, r) = point $ myrotate (pointX p1 + x, pointY p1 + y) r

trvec :: Transform -> Vec -> Vec 
trvec t v = vec (pointX newVec, pointY newVec) where
    newVec = trpoint newT (point ((vecX v), (vecY v)))
    newT = Tr $ map (\(x, y, r) -> (0 ,0, r)) (phases t)
    
transform :: Transform -> Picture -> Picture
transform t p = Pic $ map (\(p1, p2) -> (trpoint t p1, trpoint t p2)) (segments p) 