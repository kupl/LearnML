(* Exercise 1 *)
let rec merge a b =
match a with
| [] -> b
| ahead::atail -> 
(
	match b with
	| [] -> a
	| bhead::btail -> if ahead>bhead then ahead::(merge atail b) else bhead::(merge a btail)
)



