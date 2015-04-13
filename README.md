# LSH
Implementation Locality-Sensitive Hash on Haskell

See more in [Wikipedia](https://en.wikipedia.org/wiki/Locality-sensitive_hashing)

## Use

Import LSH
```haskell
>>> import qualified Data.LSH as L
```

Create new instance with `n` bands and `m` row in each band
```haskell
>>> let n = 40
        m = 5
>>> let lsh = L.new n m
```

Add instance with key `k` and value `v`
```haskell
>>> let k = 1
        v = ['a', 'b', 'c', 'd']
>>> let lsh' = L.insert k v lsh
```

Find nearest record
```haskell
>>> L.nearest ['a', 'd'] lsh' :: [Integer]
[1]
```

You can find another example in `test`
