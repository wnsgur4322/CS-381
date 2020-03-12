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

Example lists
-------------

> Ex) run ex_ifgood1
  
> Ex) run exloop_good1



list of examples:
-------------

### Conditions Example
**Description:** This example generates 
#### Good Examples
> runProg exgood_conditions
>> Expected output: Just [Four ("x",PushVN 4)]

#### Bad Examples
> runProg exbad_conditions
>> Expected output: Nothing

> runProg exbad_conditions2
>> Expected output: Nothing


> ex_ifbad2
>> Expected output: Nothing

> exloop_good1
>> Expected output: Just [Four ("LoopGood1",PushVN 6)]

> exloop_good2
>> Expected output: Just [Four ("LoopGood2",PushVN 1)]

> exloop_bad1
>> Expected output: Nothing

> exloop_bad2
>> Expected output: Nothing

> ex_dupgood
>> Expected output: Just [LeftI 4,LeftI 4]

> ex_dupbad
>> Expected output: Nothing

> ex_dropgood
>> Expected output: Just [LeftI 4]

> ex_dropbad
>> Expected output: Nothing

> ex_swapgood
>> Expected output: Just [LeftI 4,MiddleS "First"]

> ex_swapbad
>> Expected output: Nothing

> ex_overgood
>> Expected output: Just [MiddleS "First",LeftI 4,MiddleS "First"]

> ex_overbad
>> Expected output: Nothing

> ex_rotgood
>> Expected output: Just [LeftI 2,LeftI 1,LeftI 3]

> ex_rotbad
>> Expected output: Nothing

> The language usage examples
-- recursive fibonacci numbers by 'Four' language
rec_fib :: Int -> Prog
rec_fib 0 = [PushN 0]
rec_fib 1 = [PushN 0, PushN 1, Add]
rec_fib n = rec_fib (n-1) ++ rec_fib(n-2) ++ [Add]

> run (rec_fib 6)
>> Expected output: Just [LeftI 8]

-- iterative fibonacci numbers by 'Four' language
itr_fib :: Int -> Prog
itr_fib n = [PushN 0, Let("a"), PushN 1, Let("b"), PushN 0, Let("temp"), PushN 1, Let("i"), 
            Loop [PushN n, Larger] [PushN 1, Add, Bind("temp", Ref ("a")), Bind("a", Ref ("b")), Ref ("temp"), Ref ("b"), Add, Let("c"), Bind("b",Ref("c")), Drop], Drop, Drop, Drop]

> run (itr_fib 6)
>> Expected output: Just [V ("a", LeftI 8)]

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
