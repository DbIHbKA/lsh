
module Data.LSH.MinHash
    ( MinHash
    , MHRow
    , mhhash
    , new
    ) where

import           Data.Hashable
import           System.Random


type MHSalts = [Int]
type MHRow = [Int]
type MinHash = MHSalts


-- | Create new min hash
new :: Int     -- ^ Number of hashes
    -> MinHash
new n = take n salts


-- | Generate a list of Int salts. Salt will be use for
-- compute hashes
salts :: MHSalts
salts = map fst $
    scanl
        (\(_,gen) _ ->
              random gen)
        (random (mkStdGen 1)) $
    repeat ()

-- | Compute minhash hash for row
mhhash :: (Hashable a)
       => [a]
       -> MinHash
       -> MHRow
mhhash row = map
        (\salt ->
              minimum $ map (hashWithSalt salt) row)
