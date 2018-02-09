(*********************)
(*     Problem 1     *)
(*********************)
let rec max : int list -> int
= fun lst -> match lst with 
	| [] -> 0 
	| h :: t -> if(h > max(t)) then h else max(t);;

let rec min : int list -> int
= fun lst -> match lst with
	| [] -> 99999999 (*This is not ideal due to an false answer being given if the list contains only elements above 99999999*)
	| h :: t -> if(h < min(t)) then h else min(t);;
