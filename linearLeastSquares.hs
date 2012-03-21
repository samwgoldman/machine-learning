import Numeric.LinearAlgebra
import Numeric.LinearAlgebra.Util (ones, (!))
import System.Environment (getArgs)

solve :: Matrix Double -> Vector Double -> Vector Double
solve x y = pinv (x' <> x) <> (x' <> y)
    where x' = trans x

readData :: Matrix Double -> (Matrix Double, Vector Double)
readData input = (intercept ! x, y)
    where columns = reverse . toColumns $ input
          x = fromColumns . reverse . tail $ columns
          y = head columns
          intercept = ones (dim y) 1

disp :: Vector Double -> String
disp = vecdisp . dispf $ 2

main :: IO ()
main = do 
    input <- loadMatrix . head =<< getArgs
    let (x, y) = readData input
        params = solve x y
    putStr $ disp params
