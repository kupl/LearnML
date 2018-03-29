let rec drop : int list -> int -> int list
= fun l n ->
	match l with
	| [] -> []
	| hd::tl -> if n=0 then l else drop tl (n-1)

(* problem 8*)
let rec change : int list -> int -> int
= fun coins amount ->
	if amount = 0 then 1
	else if amount < 0 then 0
	else if coins = [] then 0
	else (change coins (amount - (List.nth coins 0))) + (change (drop coins 1) amount)
