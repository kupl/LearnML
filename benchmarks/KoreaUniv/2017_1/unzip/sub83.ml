(* problem 7*)

let rec unzip : ('a * 'b) list -> 'a list * 'b list
= fun lst ->
	match lst with
	| [] -> ([],[])
	| hd::tl -> match hd with
							| (a,b) -> (a::(fun (x,_) -> x) (unzip tl),b::(fun (_,x) -> x) (unzip tl));;