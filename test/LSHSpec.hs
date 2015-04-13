
module LSHSpec where

import Test.Hspec
import qualified Data.LSH as L


fill_lsh :: L.LSH Int
fill_lsh = foldr
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


spec :: Spec
spec = describe "nearest" $
    it "find nearest record" $
    L.nearest ['x', 'y', 'z', 'h'] fill_lsh `shouldBe`
    [5]
