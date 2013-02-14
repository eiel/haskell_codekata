import Data.Array

main = undefined

-- |
-- 2分探索を5種類の方法で実装する その1
-- 添字を利用して配列を中を回す感じに。
-- >>> chop 3 []
-- -1
-- >>> chop 3 [1]
-- -1
-- >>> chop 1 [1]
-- 0
-- >>> chop 1 [1, 3, 5]
-- 0
-- >>> chop 3 [1, 3, 5]
-- 1
-- >>> chop 5 [1, 3, 5]
-- 2
-- >>> chop 0 [1,3, 5]
-- -1
-- >>> chop 2 [1, 3, 5]
-- -1
-- >>> chop 4 [1, 3, 5]
-- -1
-- >>> chop 6 [1, 3, 5]
-- -1
-- >>> chop 1 [1, 3, 5, 7]
-- 0
-- >>> chop 3 [1, 3, 5, 7]
-- 1
-- >>> chop 5 [1, 3, 5, 7]
-- 2
-- >>> chop 7 [1, 3, 5, 7]
-- 3
-- >>> chop 0 [1, 3, 5, 7]
-- -1
-- >>> chop 2 [1, 3, 5, 7]
-- -1
-- >>> chop 4 [1, 3, 5, 7]
-- -1
-- >>> chop 6 [1, 3, 5, 7]
-- -1
-- >>> chop 8 [1, 3, 5, 7]
-- -1
chop :: Int -> [Int] -> Int
chop x xs = if end == -1 then -1 else bsearch array (0,end) x
  where
    array = listArray (0, end) xs
    end = length xs - 1

bsearch :: Array Int Int -> (Int, Int) -> Int -> Int
bsearch array (start,end) x
  | start > end = -1
  | x == n = half
  | x < n = bsearch array (start, half-1) x
  | x > n = bsearch array (half+1, end) x
    where
      half = (end+start) `div` 2
      n = array ! half
