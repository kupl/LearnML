(*Lee Seok Jin 2013-11417 CSE hw1_1 *)

let rec merge((l:int list),(r: int list)): int list =  
	match(l,r) with
	| (_,[]) -> l
	| ([],_) -> r
	| (lh::lt, rh::rt) -> 
		if lh > rh then lh::(merge(lt,r)) 
		else rh::(merge(l,rt))
