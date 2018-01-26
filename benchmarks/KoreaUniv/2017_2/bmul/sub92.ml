(*problem 7*)
type digit = ZERO | ONE
type bin = digit list

let rec length : bin -> int = fun b ->
match b with
|[] -> 0
|hd::tl -> 1 + (length tl);;

let rec power : int -> int = fun n ->
if n = 0 then 1
else 2 * power (n-1);;

let rec btod : bin -> int = fun b ->
match b with
|[] -> 0
|[ONE] -> 1
|[ZERO] -> 0
|ONE::tl -> (power ((length b)-1)) + (btod tl)
|ZERO::tl -> btod tl;;

let rec dtob : int -> bin = fun n ->
if n = 0 then [ZERO]
else if n = 1 then [ONE]
else if (n mod 2 = 1) then (dtob (n/2)) @ [ONE]
else (dtob (n/2)) @ [ZERO];;

let bmul : bin -> bin -> bin = fun b1 b2 ->
dtob ((btod b1) * (btod b2));;
