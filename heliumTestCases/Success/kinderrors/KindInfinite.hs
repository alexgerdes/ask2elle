-- ! This doesn't seem a valid type declaration to me
-- ! How does this pass front end?
data Tree a = Tree (a a)
