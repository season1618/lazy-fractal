import Graphics.Gloss

window :: Display
window = InWindow "Lazy Fractal" (640, 480) (100, 100)

mid :: Point -> Point -> Point
mid a b = do
    let (x1, y1) = a
    let (x2, y2) = b
    ((x1 + x2) / 2, (y1 + y2) / 2)

mix :: [a] -> [a] -> [a] -> [a]
mix [] [] [] = []
mix x y z = (head x) : (head y) : (head z) : mix (tail x) (tail y) (tail z)

triangle :: Point -> Point -> Point -> Picture
triangle a b c = line [a, b, c, a]

gasket :: Point -> Point -> Point -> [Picture]
gasket a b c = do
    let midA = mid b c
    let midB = mid c a
    let midC = mid a b

    let subA = gasket    a midC midB
    let subB = gasket midC    b midA
    let subC = gasket midB midA    c
    
    (triangle a b c) : (mix subA subB subC)

takeGasket :: Int -> Float -> [Picture]
takeGasket level len = do
    let num = sum [3^x | x <- [0..level]]
    take num (gasket (-len / 2, 0) (len / 2, 0) (0, len * sqrt 3 / 2))

main :: IO ()
main = display window white (pictures (takeGasket 10 300))