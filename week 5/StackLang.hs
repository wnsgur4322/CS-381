--
-- * Syntax of Stacking

-- Grammar for Stacking:

-- num ::= (any integer)
-- bool ::= `true` | `false`
-- prog ::= cmd*
-- cmd ::= num  push a number on the stack
--      | bool  push a boolean on the stack
--      | `add` add the top two numbers on the stack
--      | `mul` multiply the top two numbers on the
--      | `equ` check whether the top two elements are equal
--      | `if` prog `else` prog `end` if the value on the top is true, then run the frist program, else run

type Num = Int
type Prog = [Cmd] --star notation is sequnece of data

data Cmd = PushN Int
        | PushB Bool
        | Add
        | Mul Equ
        | If_Else Prog Prog
    deriving (Eq, Show)


-- 2. Write the following StackLang pogram as Haskell value:

-- 3 4 add 5 equ
--
ex1 :: Prog
ex1 = [PushN 3, PushN 4, Add, PushN 5, Equ]


-- 3.

-- 4. 

genAdd2 :: Int -> Int -> Prog
genAdd2 = undefined