-- type inference exercise
-- week 2 slide p.51

* Just :: a -> Maybe a

* not even 3
  * not even
	* not :: Bool -> Bool
	* even :: Int -> Bool
	* is not a function? -- yes!
	* does Bool == Int -> Bool? -- no!
  * 3 :: Int
  TYPE ERROR

* not (even 3) :: Bool
  * not :: Bool -> Bool
  * even 3 :: Bool
	* even :: Int -> Bool
	* 3 :: Int
	is even a function? -- yes
	does Int = Int? -- yes
	is not a function? -- yes
	does Bool = Bool -- yes

* (.) not even :: Int -> Bool
  * (.) not :: (a -> Bool) -> a -> Bool
	* (.) :: (b -> c) -> (a -> b) -> a -> c
	* not :: Bool -> Bool
	is (.) a function? -- yes
	does b -> c = Bool -> Bool -- yes
		{ b = Bool, c = Bool } 
  * even :: Int -> Bool
    is `(.) not` a function? -- yes
    does a -> Bool = Int -> Bool -- yes
	{a = Int}

* even . not -> TYPE ERROR

* map (Just . even)
  * map :: (a -> b) -> [a] -> [b]

  * Just . even :: Int -> Maybe Bool
	* Just :: c -> Maybe
	* even :: Int -> Bool
	coonection : c =? Bool { c = Bool }
	input to right function: Int
	output of left function: Maybe Bool

	is map a function? -- yes
	a -> b =? Int -> Maybe Bool {a = Int, b = Maybe Bool}

	result type of map: [Int] -> [Maybe Bool]

difficulty: 2~5 and one tircky Q like 6 in example.