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


## Recursion/loops Example
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


## Procedures/functions with arguments Example
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


## Stack manipulation operations Example
**Description:** This example generates 

#### Good Examples
> runProg 
>> Expected output:

> runProg 
>> Expected output: 

#### Bad Examples
> runProg 
>> Expected output: Error

>> Reason:  


> runProg 
>> Expected output: Error

>> Reason: 


## Strings and operations Example
**Description:** This example generates 

#### Good Examples
> runProg 
>> Expected output:

> runProg 
>> Expected output: 

#### Bad Examples
> runProg 
>> Expected output: Error

>> Reason:  


> runProg 
>> Expected output: Error

>> Reason: 



## List/array data type and operations Example
**Description:** This example generates 

#### Good Examples
-- list operations
-- list creation
> runProg (int_list [1,2,3])
>> Expected output: Just [LeftI 3,LeftI 2,LeftI 1]

-- list concatenation
> runProg (list_concatenation [1,2,3] [4,5,6])
>> Expected output: Just [LeftI 6, LeftI 5, LeftI 4, LeftI 3, LeftI 2, LeftI 1]


#### Bad Examples
> runProg 
>> Expected output: Error

>> Reason:  


> runProg 
>> Expected output: Error

>> Reason: 


## Some other feature of your choice Example
**Description:** This example generates 

#### Good Examples
> runProg 
>> Expected output:

> runProg 
>> Expected output: 

#### Bad Examples
> runProg 
>> Expected output: Error

>> Reason:  


> runProg 
>> Expected output: Error

>> Reason: 
