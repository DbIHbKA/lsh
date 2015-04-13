
module Data.LSH where

import           Data.Foldable    (foldr)
import           Data.Hashable
import           Data.List.Split
import qualified Data.LSH.MinHash as MH
import qualified Data.Map         as M
import qualified Data.Set         as S
import           Prelude          hiding (foldr)


data LSH k = LSH
    { lshBand :: Int
    , lshRow  :: Int
    , lshMH   :: MH.MinHash
    , lshDB   :: M.Map Int (S.Set k)
    }


-- | Create LSH
new :: Int  -- ^ Number of band
    -> Int  -- ^ Number of row
    -> LSH k
new band row = LSH
    { lshBand = band
    , lshRow = row
    , lshMH = MH.new (band * row)
    , lshDB = M.empty
    }


-- | Insert a row to an LSH
insert :: (Hashable a,Ord k)
       => k -> [a] -> LSH k -> LSH k
insert key value lsh = lsh
    { lshDB = updateLSHDB
    }
    where updateLSHDB = foldr
                  (\chk m ->
                        M.insertWith
                            S.union
                            (hash chk)
                            (S.singleton key)
                            m)
                  (lshDB lsh) $
              chunksOf
                  (lshRow lsh)
                  (MH.mhhash value (lshMH lsh))


-- | Search nearest rows for given
nearest :: (Hashable a, Ord k)
           => [a]
           -> LSH k
           -> [k]
nearest value lsh = S.toList $
    foldr
        (\chk res ->
              case M.lookup
                       (hash chk)
                       (lshDB lsh) of
                  Just s -> S.union s res
                  Nothing -> res)
        S.empty $
    chunksOf
        (lshRow lsh)
        (MH.mhhash value (lshMH lsh))
