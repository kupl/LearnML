(* problem 8*)

let rec change : int list -> int -> int
= fun coins amount ->
let rec last_del lit =
	match lit with
	| []->[]
	| [a]->[]
	| hd::tl -> hd::(last_del tl)
in
let rec last_ele lit =
	match lit with
	| []->0
	| [a]->a
	| hd::tl -> last_ele tl
in
if amount == 0 then 1
else if amount < 0 then 0
else if coins == [] then 0
else (change (last_del coins) amount) + (change coins (amount-(last_ele coins)));;

