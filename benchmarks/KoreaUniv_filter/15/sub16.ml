(*********************)
(* Problem 1: filter *)
(*********************)
let rec filter pred lst = 
let answer = [] in
match lst with
[] -> answer
| hd::tl -> if (pred hd) then hd::(filter pred tl)@answer else filter pred tl
	