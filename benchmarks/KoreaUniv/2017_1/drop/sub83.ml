(* problem 6*)

let rec drop : 'a list -> int -> 'a list
= fun l n -> if n=0 then l 
  					 else if n<0 then raise (Failure "n should be larger than 0")
						 else match l with 
  								| [] -> []
  								| hd::tl -> drop tl (n-1);;