(* 2006-11782 Song Young-chan, Hw1-4 zipperN *)

let rec zipperN(input:int list list) =
	match input with
	 [] -> []
	|[]::remain -> zipperN(remain)
	|first::[] -> first
	|first::remain -> (List.hd first)::(zipperN(remain@[List.tl first]))
