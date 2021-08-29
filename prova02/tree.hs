data Tree a = Leaf | 
    Branch (Tree a) a (Tree a)
    deriving Show

atravessar :: (a -> b) -> Tree a -> Tree b
atravessar fun Leaf = Leaf
atravessar fun (Branch tl x tr) = Branch (atravessar fun tl) (fun x) (atravessar fun tr)


{-- 

caso de teste   => atravessar (*4) ( Branch (Branch (Leaf) 2 (Leaf) ) 2 (Leaf) )
resultado       => Branch (Branch Leaf 8 Leaf) 8 Leaf

--}