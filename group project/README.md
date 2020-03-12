# CS381 FinalProject

Team members
-------------
> Junhyeok Jeong, jeongju@oregonstate.edu

> Youngjoo Lee, leey3@oregonstate.edu

> Ethan Mendelson, mendelse@oregonstate.edu



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

## Conditions Example
**Description:** This example generates final grades with your score (Integer n). Depends on your grade (n), it will generate 'A' to 'F' based on standard grading ranges (A >= 93, A >= 90, B+ >= 87, B >= 83, B- >= 80, C+ >= 77, C >= 73, C- >= 70, D >= 60, F < 60)  

#### Good Examples
> runProg grades 81
>> Expected output: Just [V ("result",MiddleS "B-"),V ("score",LeftI 81)]

> runProg grades 97
>> Expected output: Just [V ("result",MiddleS "A"),V ("score",LeftI 97)]


#### Bad Examples
> runProg grades "A"
>> Expected output: Error

>> Reason: The input of grades can't be String because string isn't Integer value. Therefore, it occurs "Type error".

> runProg grades 83.5
>> Expected output: Error

>> Reason: Our language can't handle "Float" and "Double" types which includes decimal points. Therefore, it occurs "Type error"


## Recursion/loops Example
**Description:** This example generates Fibonacci numbers with specific number of 'n'. We made two functions which can simply generate Fibonacci numbers on the screen with two diffent ways (recursive / iterative).
#### Good Examples

-- recursive fibonacci numbers by 'Four' language
> run (rec_fib 6)
>> Expected output: Just [LeftI 8]

-- iterative fibonacci numbers by 'Four' language
> runProg (itr_fib 6)
>> Expected output: Just [LeftI 8]

#### Bad Examples




> The language usage examples


-- factorial calculation by 'Four' language
factorial :: Int -> Prog
factorial n = [PushN 1, Let("fact"), PushN 1, Let ("temp"),
              PushN 1, Let("i"), Loop [PushN n, Larger]
              [PushN 1, Add, Ref ("temp"), PushN 1, Add, Let ("temp2"), Bind("temp", Ref ("temp2")), Drop, Ref ("fact"), Ref ("temp"), Mul, Let("mul"), Bind("fact", Ref ("mul")), Drop], Drop, Drop]

>run (factorial 5)
>> Expected output: Just [V ("fact",LeftI 720)]

-- list operations
-- list creation
> run (int_list [1,2,3])
>> Expected output: Just [LeftI 3,LeftI 2,LeftI 1]

-- list concatenation
> run (list_concatenation [1,2,3] [4,5,6])
>> Expected output: Just [LeftI 6, LeftI 5, LeftI 4, LeftI 3, LeftI 2, LeftI 1]
