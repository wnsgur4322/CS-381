# CS381 FinalProject

Team members
-------------
> Junhyeok Jeong, jeongju@oregonstate.edu

> Youngjoo Lee, leey3@oregonstate.edu

> Ehtan Mendelson, mendelse@oregonstate.edu

Introduction
-------------
> Four is a stack-based language with four input types for constructing and calculating mathmatical problems (Integer, Bool, String, and Name with each type). 



How to run examples 
-------------

> "**ghci group_project.hs**" in the directory which has "group_project.hs"

> After above, "**runProg** [**example names**]"

> Ex) runProg exgood_conditions
  
> Ex) runProg exbad_conditions


List of Examples
-------------

### Conditions Example
how to use: [True or False] **IfElse** [if True] [if False]

e.g.) [3, 4, Larger, IfElse, [6] [5]] -> [5]

**Description:** This example generates final grades with your score (Integer n). Depends on your grade (n), it will generate 'A' to 'F' based on standard grading ranges (A >= 93, A >= 90, B+ >= 87, B >= 83, B- >= 80, C+ >= 77, C >= 73, C- >= 70, D >= 60, F < 60)  

#### Good Examples
> runProg (grades 81)
>> Expected output: Just [V ("result",MiddleS "B-"),V ("score",LeftI 81)]

> runProg (grades 97)
>> Expected output: Just [V ("result",MiddleS "A"),V ("score",LeftI 97)]


#### Bad Examples
> runProg (grades "A")
>> Expected output: Error

>> Reason: The input of grades can't be String because string isn't Integer value. Therefore, it occurs "Type error".

> runProg (grades 83.5)
>> Expected output: Error

>> Reason: Our language can't handle "Float" and "Double" types which includes decimal points. Therefore, it occurs "Type error"


### Recursion/loops Example
How to use: [Integer or Boolean value with name / False] **Loop** [Condition check -> True or False] [Process when False]

e.g. [("i", 3), Loop, [3, Larger], [1, Add]] -> [("i", 4)]

**Description:** This example generates Fibonacci numbers with specific number of 'n'. We made two functions which can simply generate Fibonacci numbers on the screen with two diffent ways (recursive / iterative).
#### Good Examples

-- recursive fibonacci numbers by 'Four' language
> runProg (rec_fib 6)
>> Expected output: Just [LeftI 8]

-- iterative fibonacci numbers by 'Four' language
> runProg (itr_fib 6)
>> Expected output: Just [LeftI 8]

#### Bad Examples
> runProg (rec_fib "A")
>> Expected output: Error

>> Reason:  The input of grades can't be String because string isn't Integer value. Therefore, it occurs "Type error".


> runProg (itr_fib 6.5)
>> Expected output: Error

>> Reason: Our language can't handle "Float" and "Double" types which includes decimal points. Therefore, it occurs "Type error"


### Procedures/functions with arguments Example
How to use: [Value(String, integer, or boolean (e.g 2)), Let ("Name")] -> [("Name", 2)]
If there is an value with name: [Ref("Name")] --> [Value] from "Name" -> e.g.) [Ref("Name")] -> [2]
If want to change value in "Name": [Bind("Name", value you want to change (e.g. 3))] -> [("Name", 3)]
(or Bind("Name", Ref("OTHER"))) -> [("Name", 5), ("OTHER", 5)]
**Description:** This example generates Factorial numbers using reused value through calling its name by "Ref". In "factorial" function, it used "Ref" to use its value without name to refer to its value to calculate factorial numbers with 'n'

(Ref only can be used for 'V (name, value) which has value name with its value')

#### Good Examples
> runProg (factorial 6)
>> Expected output: Just [V ("fact",LeftI 5040)]

> runProg (factorial 10)
>> Expected output: Just [V ("fact",LeftI 39916800)]

#### Bad Examples
> runProg (factorial "A")
>> Expected output: Error

>> Reason:  The input of grades can't be String because string isn't Integer value. Therefore, it occurs "Type error".


> runProg (factorial 6.5)
>> Expected output: Error

>> Reason: Our language can't handle "Float" and "Double" types which includes decimal points. Therefore, it occurs "Type error"


### Stack manipulation operations Example
How to use: [Value, Dup] -> [Value, Value]
[Value, Drop] -> []
[v1, v2, Swap] -> [v2, v1]
[v1, v2, Over] -> [v1, v2, v1]
[v1, v2, v3, Rot] -> [v2, v3, v1]

**Description:** Generally, Stack manipulation operations are used in Fibonacci and factorial, but it's hard to show how the user just uses Stack maniuplation operations. Thus, we made some sample examples of them to show it works and not work when.

#### Good Examples
**Description:** This example generates Double with "Dup" and "Mul".
> runProg (double_value 2)
>>double_value :: Int -> Prog

>> double_value n = [PushN n, Dup, Mul]

>> Expected output: Just [LeftI 4]

**Description:** This example generates Triple with "Dup", "Over", and "Mul".
> runProg (triple_value 3)

>> triple_value :: Int -> Prog

>> triple_value n = [PushN n, Dup, Over, Mul, Mul]

>> Expected output: Just [LeftI 27]

#### Bad Examples
**Description:** Generally, Stack manipulation oeprations can't make bad examples because 'Dup', 'drop', 'swap', 'over', 'rot' can be applied in any situation without duplication with value name, empty stack or lacking inputs in stack. Thus, there is one example with trying duplication value with name, one example in empty stack, and one example lacking input numbers in stack for 'swap', 'rot', or 'over'.  

> runProg error_dup 
>> error_dup :: Prog

>> error_dup = [PushN 3, Let ("a"), Dup]

>> Expected output: Nothing

>> Reason:  As I said before, make value with same name which showed up before, so 'Dup' with value with name can't be approved.

> runProg (empty_stack) 
>> empty_stack :: Prog

>> empty_stack = [Dup]

>> Expected output: Nothing

>> Reason:  Duplication can't apply to Empty stack because duplication needs at least one value before calling 'Dup'.


> runProg lack_input
>> lack_input :: Prog

>> lack_input = [PushN 3, Swap]

>> Expected output: Nothing

>> Reason: To swap values, needs two values which will be swapped before calling 'Swap'


### Strings and operations Example
**Description:** This example generates clear shows the result of 'IfElse' like "Result: True" or "Result: False" using concatenation of string values. It also works with value with name.

#### Good Examples
> runProg string_con1
>> Expected output: Just [MiddleS "3 is bigger than 4? -> Result: False"]

> runProg string_con2
>> Expected output: Just [MiddleS "3 is smaller than 4? -> Result: True"]

> runProg string_con3
>> Expected output: Just [V ("Question",MiddleS "3 is smaller than 4? -> Result: True")]

#### Bad Examples
> runProg bad_string_con1 
>> bad_string_con1 :: Prog

>> bad_string_con1 = [PushS "3 is bigger than 4? -> ", PushS "Result: ", Add, PushN 3, PushN 4, Larger, IfElse [PushB True] [PushB False], Add]

>> Expected output: Nothing

>> Reason:  String operations can't work with other types of value (integers or bool)


> runProg bad_string_con2 
>> bad_string_con2 :: Prog

>> bad_string_con2 = [PushS "What is 3 + 4? -> ", PushS "Result: ", Add, PushN 3, PushN 4, Add, Add]

>> Expected output: Nothing

>> Reason: String operations can't work with other types of value (integers or bool) 



### List/array data type and operations Example
**Description:** This example generates list creation and concatenation after creating two lists uisng 'int_list'.

#### Good Examples
-- list operations

-- list creation
> runProg (int_list [1,2,3])
>> Expected output: Just [LeftI 3,LeftI 2,LeftI 1]

-- list concatenation
> runProg (list_concatenation [1,2,3] [4,5,6])
>> Expected output: Just [LeftI 6, LeftI 5, LeftI 4, LeftI 3, LeftI 2, LeftI 1]


#### Bad Examples
> runProg (int_list ["a","b","c"])
>> Expected output: Error

>> Reason: In our language, Integer value only can create list (not other types: string and bool). Threrefore, it occurs error when you use 'int_list' with string or bool type values


> runProg (list_concatenation ["a","b","c"] [4,5,6]) 
>> Expected output: Error

>> Reason: In our language, list concatenation also can be handled by integer values (not other types: string and bool). Threrefore, it occurs error with string or bool type values


### Some other feature of your choice Example
**Description:** Fibonacci and Factorial examples also uses "Bind" with "Ref" to change value with names (By "Let"). In these examples, just to make clear, there are some examples how "Bind" with "Ref" and "Bind without "Ref" work. ("Bind" and "Let" for the user who uses this language are used to construct fibonacci, factorial, and other mathmatics features, so below examples just shows how to use it and features of "Bind" and "Let")

#### Good Examples

> runProg good_bind1
>> Explanation: This example generate a  = 4; a = 3; through "Bind" without "Ref" how it works.

>> Expected output: Just [V ("a",LeftI 3)]

> runProg good_bind2
>> Explanation: This example generates a = 4; b = 3; a = b; through "Bind" and "Ref".

>> Expected output: Just [V ("b",LeftI 3),V ("a",LeftI 3)]

#### Bad Examples

> runProg bad_bind1
>> Explanation: This example shows error when put String value into Integer value with name like: a = 4; a = "Hi";

>> Expected output: Nothing

>> Reason: After deciding types of value with name, the type cannot be changed. Therefore, it's error.


> runProg bad_bind2
>> Explanation: This example shows error when put String value with name into Integer value with name like: a = 4; b = "Hi"; a = b;

>> Expected output: Nothing

>> Reason: After deciding types of value with name, the type cannot be changed. Therefore, it's error.

> runProg bad_let
>> Explanation: This example shows error when make 'Let' with same names ('Let' only permits name that doesn't show up yet).

>> Expected output: Nothing

>> Reason: After a = 4 through 'Let', we can't a = 3 through 'Let', can only using 'Ref' to change value in 'a'