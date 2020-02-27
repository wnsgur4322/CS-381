# CS381 FinalProject

Team members
> Junhyeok Jeong, jeongju@oregonstate.edu

> Youngjoo Lee, leey3@oregonstate.edu

> Ethan Mendelson, mendelse@oregonstate.edu

Four is a stack-based language with four input types
> (Integer, Bool, String, and Name with each type). 

To run examples => run <example names> after "ghci group_project.hs"
> Ex) run ex_ifgood1
  
> Ex) run exloop_good1

list of examples:
> ex_ifgood1
>> Expected output: Just [Four ("x",PushVN 4)]

> ex_ifgood2
>> Expected output: Just [RightB True]

> ex_ifbad1
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
