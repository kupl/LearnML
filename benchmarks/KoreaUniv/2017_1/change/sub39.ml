(*problem 8*)
let rec length l =
	match l with
		| [] -> 0
		| h::t -> length t + 1

let head l =
	match l with
		| [] -> 0
		| h::t -> h

let tail l =
	match l with
		| [] -> []
		| h::t -> t

let rec change : int list -> int -> int
= fun coins amount ->
	if(amount = 0) then 1
	else if(amount < 0) then 0
	else if(length coins = 0) then 0
	else (change coins (amount - head coins)) + (change (tail coins) amount)
