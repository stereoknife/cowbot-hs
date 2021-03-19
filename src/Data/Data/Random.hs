module Data.Data.Random where

import           Data.Data     (Constr, Data, DataType, fromConstr, indexConstr,
                                maxConstrIndex)
import           System.Random (Random (randomR), getStdRandom)

randomCons :: DataType -> IO Constr
randomCons d = do
    rn <- getStdRandom $ randomR (1, maxConstrIndex d)
    return $ indexConstr d rn

randomValue :: Data a => DataType -> IO a
randomValue d = do
    cons <- randomCons d
    return $ fromConstr cons
