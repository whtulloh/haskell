data ExprT = Lit Int 
          | Add ExprT ExprT
          | Mul ExprT ExprT
     deriving (Show, Eq)

-- > 2 + 3
-- ghci> Add (Lit 2) (Lit 3)
-- expected: Add (Lit 2) (Lit 3)

-- > 5 * 4
-- ghci> Mul (Lit 5) (Lit 4)
-- expected: Mul (Lit 5) (Lit 4)

-- > (2 + 3) * 4
-- ghci> Mul (Add (Lit 2) (Lit 3)) (Lit 4)
-- expected: Mul (Add (Lit 2) (Lit 3)) (Lit 4)

eval :: ExprT -> Int
eval (Lit x) = x
eval (Mul x y) = (eval x) * (eval y)
eval (Add x y) = (eval x) + (eval y)
-- ghci> eval (Lit 2)
-- expected: 2
-- ghci> eval (Add (Lit 2) (Lit 3))
-- expected: 5
-- ghci> eval (Mul (Add (Lit 2) (Lit 3)) (Lit 4))
-- expected: 20