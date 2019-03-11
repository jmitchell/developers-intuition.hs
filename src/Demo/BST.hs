module Demo.BST where

data BST k v = Leaf | Branch (BST k v) k v (BST k v)
  deriving Eq

instance (Show k, Show v) => Show (BST k v) where
  show = show . toList

fromList :: Ord k => [(k, v)] -> BST k v
fromList = foldr (\(k,v) -> insert k v) Leaf

toList :: BST k v -> [(k, v)]
toList Leaf             = []
toList (Branch l k v r) = toList l ++ [(k, v)] ++ toList r

insert :: Ord k => k -> v -> BST k v -> BST k v
insert k v Leaf = Branch Leaf k v Leaf
insert k v (Branch l k' v' r)
  | k < k'  = Branch (insert k v l) k' v' r
  | k > k'  = Branch l k' v' (insert k v r)
{-
  | k == k' = Branch l k' v' r   -- subtle bug to illustrate why you
                                 -- "don't let Satan write your unit
                                 -- tests"
-}
  | k == k' = Branch l k' v r

keys :: BST k v -> [k]
keys Leaf             = []
keys (Branch l k _ r) = keys l ++ [k] ++ keys r

deleteKey :: Ord k => k -> BST k v -> BST k v
deleteKey _ Leaf = Leaf
deleteKey k (Branch l k' v r)
  -- Forgive the inefficient implementation; the focus here is on
  -- testing the correctness of `insert`.
  | k == k' = fromList (toList l ++ toList r)
  | k < k'  = Branch (deleteKey k l) k' v r
  | k > k'  = Branch l k' v (deleteKey k r)
