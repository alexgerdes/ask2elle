module ExprDoLetDupVar where



main = do { let { x = 0; x = 1 } ; x}