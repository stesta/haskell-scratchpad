{-# LANGUAGE OverloadedStrings #-}

class (Eq a, Enum a, Bounded a) => CyclicEnum a where
    cpred :: a -> a
    cpred d 
        | d == minBound = maxBound 
        | otherwise = pred d

    csucc :: a -> a
    csucc d 
        | d == maxBound = minBound 
        | otherwise = succ d


data Direction = North | East | South | West
    deriving (Eq, Enum, Bounded, Show)

instance CyclicEnum Direction


data Turn = TNone | TLeft | TRight | TAround
    deriving (Eq, Enum, Bounded, Show)

instance Semigroup Turn where
    TNone <> t = t 
    TLeft <> TLeft = TAround
    TLeft <> TRight = TNone
    TLeft <> TAround = TRight
    TRight <> TRight = TAround
    TRight <> TAround = TLeft
    TAround <> TAround = TNone
    t1 <> t2 = t2 <> t1

instance Monoid Turn where
    mempty = TNone

every :: (Enum a, Bounded a) => [a]
every = enumFrom minBound 

rotate :: Turn -> Direction -> Direction
rotate TNone = id
rotate TLeft = cpred
rotate TRight = csucc
rotate TAround = cpred . cpred

rotateMany :: Direction -> [Turn] -> Direction
rotateMany = foldl (flip rotate)

rotateMany' :: Direction -> [Turn] -> Direction
rotateMany' d ts = rotate (mconcat ts) d

rotateManyStep :: Direction -> [Turn] -> [Direction]
rotateManyStep = scanl (flip rotate)


orient :: Direction -> Direction -> Turn
orient d1 d2 = head $ filter (\t -> rotate t d1 == d2) every

orientMany :: [Direction] -> [Turn]
orientMany ds@(_:_:_) = zipWith orient ds (tail ds)
orientMany _ = []


-- rotateFromFile :: Direction -> FilePath -> IO ()
-- orientFromFile :: FilePath -> IO ()
-- main :: IO ()