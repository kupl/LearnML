(*problem 6*)
let rec drop : 'a list -> int -> 'a list = fun l n 
-> if n >= List.length l then [] 
	else if n=1 then List.tl l 
		else drop (List.tl l) (n-1);;