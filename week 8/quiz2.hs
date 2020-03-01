module Quiz2 where

-- example semantic from other languages
-- c++
-- int main() {
--    int x = 0, y;
--    while (x < 5) {
--        x++;
--        y = x;
--    }
-- }

-- python
-- x = 5  -- binding
-- y = 1  -- binding
-- while x > 0:
--    y = y * x
--    x -= 1

-- print(y)

type Floop = Int                -- bind 'Floop' to Int
nats :: [Int]
nats = 2 : map (+1) nats        -- map references (+1), nats references nats

primes :: [Int]
primes = primes' nats
primes' (x:xs) = x : (primes' $ filter ((/= 0) . (`mod` x)) xs)

-- Semantic Domain

-- What is k'? 5, 9

k' = let x = 9
         y = x
    in let x = 5
           z = y 
        in (x,y)

-- scope of k'
-- k' = {let x = 9
--         y = x
--    in ( let x = 5
--           z = y 
--        in (x,y))
--     }

-- Circle names, underline references:
-- names: x,y,triple, double, perimeter, +
-- for +, built in function
-- (+) :: Int -> Int -> Int 
triple :: Int -> Int
triple y = double y + y

double :: Int -> Int
double x = x + x

perimeter :: Int -> Int -> Int
perimeter x y = double x + double y

-- language with names:
type Name = String
data Val = Vb Bool
         | Vi Int
         deriving (Eq, Show)

type Error = String
ex = Add (Get "x") (Lit 2) -- Env -> Maybe Val ?

-- [("x", Vi 5)], x + x + x + y
-- type Env = Map Name Val
-- type Env = [(Name, Val)]
-- type Env = Name -> Value

data Expr = Lit Int
          | Get Name -- Get name
          | Add Expr Expr -- Add two values
          | Same Expr Expr -- ==

data Stmt = ProgN [Stmt]
          | Set Name Expr
          | If Expr Stmt Stmt
          | Done Expr
-- Domain = Env -> Either Val Env

type Env = [(Name, Val)]

-- lookup :: String -> [(String, a)] -> Maybe a
-- In Python: dict[name]

semE :: Expr -> Env -> Maybe Val
semE (Lit i) _ = Just (Vi i)
semE (Get n) env = lookup n env
semE (Add l r) env =
    case (semE l env, semE r env) of
        (Just (Vi lv), Just (Vi rv)) -> Just (Vi (lv+rv))
        _ -> Nothing
    
-- option 2
--    do
--        lv <- semE l env
--        rv <- semE r env
--        return (lv+rv)
-- option 3
--     [lv + rv | lv <- semE lv env, rv <- semE r env]

semE (Same l r) env =
    case (semE l env, semE r env) of
        (Just (Vi lv), Just (Vi rv)) -> Just (Vb (lr == rv))
        (Just (Vb lv), Just (Vb rv)) -> Just (Vb (lr == rv))
        _ -> Nothing

semS :: Stmt -> Env -> Either Val Env
semS (ProgN []) env = Right env
semS (ProgN (s:ss)) env =
    case semS s env of
        L v ->
        

type Prog = [Expr]

-- What is environment?
-- type Env = [(Name, Val)]
-- What is the semantic domain of expressions?
-- type Domin = 


-- Stack based
data Cmd
    = PushInt Int
    | Pop
    | AddCmd

type Stack = [Int]
type Domain = Stack -> Maybe Stack

sem :: Cmd -> Domain
sem (PushInt i) = \s -> Just (i:s)
sem Pop = \s -> 
            case s of
                [] -> Nothing
                (_:xs) -> Just xs
sem AddCmd = \s ->
            case s of
                [] -> Nothing
                [x] -> Nothing
                (x:y:xs) -> Just (x+y) : xs
