(*********************)
(* Problem 1: filter *)
(*********************)
let rec filter pred lst = 
	match lst with
		[]->[]
		|hd::tl->if pred hd then hd::(filter pred tl) else (filter pred tl);;
		
