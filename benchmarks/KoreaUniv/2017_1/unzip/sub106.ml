
(* problem 7*)

let unzip : ('a * 'b) list -> 'a list * 'b list
= fun lst -> let l1 = [] in
             let l2 = [] in
	     let rec f = fun lst l1 l2 -> match lst with 
	                              |[] -> (l1, l2)
				      |h::t -> match h with
			                     |(x, y) -> f t (l1@[x]) (l2@[y]) in

             f lst l1 l2
