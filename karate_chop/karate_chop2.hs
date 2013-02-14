import Data.Array

main = undefined

-- |
-- 2分探索を5種類の方法で実装する その2
-- あらかじめ位置情報をくっつけておいて探索する配列を小さくしてけば必要な引数が減らせる。
-- しかし、 配列の一部からrangeを指定してまとめて取得する方法を用意しないと効悪そう。どうなんだろ。ixmap id で代用した。
-- ベンチマークをとる方法を用意したほうがよさそ。
-- C言語ならアドレスから逆算できるよね。
--
-- しかし、実装はよりごちゃごちゃしていいところあんまりない。
-- 範囲チェックの場所がかわっただけな気もする。
-- startかendか気にしない範囲チェックにすればコード量が減らせる。
-- inRange の戻り値とかが Maybe とか Either モナドならそれはそれでシンプルにかけるのかな。
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
chop x xs = if end == -1 then -1 else bsearch array x
  where
    array = listArray (0, end) $ zip xs [0..]
    end = length xs - 1

bsearch :: Array Int (Int,Int) -> Int -> Int
bsearch array x
  | x == value = index
  | x < value = bsearch' (start , half-1)
  | x > value = bsearch' (half+1, end)
    where
      arrayBounds = bounds $ array
      (start,end) = arrayBounds
      half = (start+end) `div` 2
      (value, index) = array ! half
      inArrayRange = inRange arrayBounds
      bsearch' range@(s,e) = if s > e
                             then -1
                             else bsearch (ixmap range id array) x
