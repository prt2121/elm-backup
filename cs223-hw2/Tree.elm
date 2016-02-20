module Tree where
-- BST?
type Tree = Empty | Node Int Tree Tree

-- ex 2.2

-- 2.2.1
member : Int -> Tree -> Bool
member n t =
  case t of
    Empty -> False
    Node i l r ->
      case compare i n of
        LT -> member n r
        EQ -> True
        GT -> member n l

-- 2.2.2
fullTree : Int -> Int -> Tree
fullTree n h =
  if h <= 0
     then Empty
     else let t = fullTree n (h - 1)
          in Node n t t
