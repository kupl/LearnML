
(* problem 7*)
type digit = ZERO | ONE
type bin = digit list

let rec make a =
match a with
| 0 -> [ZERO]
| 1 -> [ONE]
| _ -> (make (a/2))@(make (a mod 2));;

let rec length l = 
match l with
| [] -> 0
| hd::tl -> 1+(length tl);;

let rec expt b n =
if n = 0 then 1
else b*(expt b (n-1));;

let rec solve lst n
= match lst with
| [] -> 0
| hd::tl ->
if hd = ONE then (expt 2 (n-1))+(solve tl (n-1))
  else (solve tl (n-1));;

let bmul : bin -> bin -> bin
= fun b1 b2 -> make((solve b1 (length b1))*(solve b2 (length b2)));;
