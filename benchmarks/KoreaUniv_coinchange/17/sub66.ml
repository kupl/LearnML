(* problem 8*)
let rec change : int list -> int -> int
= fun coins amount -> if amount=0 then 1
	else match coins with
	| [] -> 0
	| hd::tl -> if hd<=amount then (change coins (amount-hd)) + (change tl amount)
	else change tl amount