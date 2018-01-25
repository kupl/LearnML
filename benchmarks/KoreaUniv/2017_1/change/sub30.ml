(* problem 8*)
let rec insert a l = 
	match l with 
	| [] -> [a]
	| hd::tl -> if a > hd then a::hd::tl else hd::(insert a tl)

let rec sort l =
	match l with
	| [] -> []
	| hd::tl -> insert hd (sort tl)

let rec calc coins amount = 
	if amount = 0 then 1
	else if amount <0 then 0
	else match coins with
	| [] -> 0
	| hd::tl -> if hd <= 0 then 0 
		else ((calc tl amount) + (calc coins (amount-hd)))

let change : int list -> int -> int
= fun coins amount -> 
	if amount = 0 then 1
	else if amount < 0 then 0
	else if coins = [] then 0
	else calc (sort coins) amount


