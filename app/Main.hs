module Main where
import Data.Matrix

-- | Split a matrix into four quadrants
splitMatrix :: Matrix Int -> (Matrix Int, Matrix Int, Matrix Int, Matrix Int)
splitMatrix mtrx = (c11, c12, c21, c22)
    where
        n = nrows mtrx
        m = n `div` 2
        (c11, c12, c21, c22) = splitBlocks m m mtrx

-- | Combine four matrices into one
combineMatrix :: (Matrix Int, Matrix Int, Matrix Int, Matrix Int) -> Matrix Int
combineMatrix (c11, c12, c21, c22) = (c11 <|> c12) <-> (c21 <|> c22)

-- | Strassen's matrix multiplication algorithm
strassen :: Matrix Int -> Matrix Int -> Matrix Int
strassen a b 
    | n < 32 = multStd a b
    | otherwise = combineMatrix (c11, c12, c21, c22)
    where
        n = nrows a
        (a11, a12, a21, a22) = splitMatrix a
        (b11, b12, b21, b22) = splitMatrix b

        p1 = strassen (a11 + a22) (b11 + b22)
        p2 = strassen (a21 + a22) b11
        p3 = strassen a11 (b12 - b22)
        p4 = strassen a22 (b21 - b11)
        p5 = strassen (a11 + a12) b22
        p6 = strassen (a21 - a11) (b11 + b12)
        p7 = strassen (a12 - a22) (b21 + b22)

        c11 = p1 + p4 - p5 + p7
        c12 = p3 + p5
        c21 = p2 + p4
        c22 = p1 - p2 + p3 + p6

main :: IO ()
main = do
    let a = matrix 4 4 $ \(i,j) -> 4
    let b = matrix 4 4 $ \(i,j) -> 4
    print(strassen a b)
