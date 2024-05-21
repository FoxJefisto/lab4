module Main where
import Data.Matrix
import Control.Parallel.Strategies
import System.Clock
import Control.DeepSeq
import System.Environment

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

        (c11, c12, c21, c22) = runEval $ do
            p1 <- rpar $ strassen (a11 + a22) (b11 + b22)
            p2 <- rpar $ strassen (a21 + a22) b11
            p3 <- rpar $ strassen a11 (b12 - b22)
            p4 <- rpar $ strassen a22 (b21 - b11)
            p5 <- rpar $ strassen (a11 + a12) b22
            p6 <- rpar $ strassen (a21 - a11) (b11 + b12)
            p7 <- rpar $ strassen (a12 - a22) (b21 + b22)
            rdeepseq p1
            rdeepseq p2
            rdeepseq p3
            rdeepseq p4
            rdeepseq p5
            rdeepseq p6
            rdeepseq p7
            p11 <- rpar (p1 + p4 - p5 + p7)
            p12 <- rpar (p3 + p5)
            p21 <- rpar (p2 + p4)
            p22 <- rpar (p1 - p2 + p3 + p6)
            rdeepseq p11
            rdeepseq p12
            rdeepseq p21
            rdeepseq p22
            return (p11, p12, p21, p22)

writeMatrixToFile :: FilePath -> Matrix Int -> IO ()
writeMatrixToFile filePath matrix = writeFile filePath $ prettyMatrix matrix

main :: IO ()
main = do
    args <- getArgs
    let size = read (args !! 0) :: Int

    timeStart <- getTime Monotonic
    result <- return $!! (strassen (matrix size size $ \(i,j) -> 4) (matrix size size $ \(i,j) -> 4))
    timeEnd <- getTime Monotonic

    let timeRun = timeEnd - timeStart
    let seconds = sec timeRun
    let milliseconds = (nsec timeRun) `div` 1000000
    print $ "Time run: " ++ show seconds ++ "." ++ show milliseconds

    writeMatrixToFile "result.txt" result
    return ()
