
module ClearTest where

import qualified Data.LSH as L


fill :: L.LSH Int
fill = foldr
        (\(k,v) lsh ->
              L.insert k v lsh)
        (L.new 20 2)
        [ (1, ['a', 'b', 'c', 'd'])
        , (2, ['a', 'b', 'd'])
        , (3, ['a', 'b', 'e', 'd'])
        , (4, ['w', 'x', 'y', 'z'])
        , (5, ['x', 'y', 'z'])
        , (6, ['w', 'x', 'q', 'z', 'y'])
        , (7, ['r', 's', 't'])
        , (8, ['u', 's', 't'])]


main :: IO ()
main = do
  print $ L.nearest ['x', 'y', 'z'] fill
  print $ L.nearest ['a', 'b', 'c'] fill
