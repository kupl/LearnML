(* Problem 3 *)
let rec sieve : int * int -> bool (*for divide n *)
= fun (n,d)-> if n=d then true
						  else if(n mod d =0) then false 
							else  sieve (n,d+1);;

let rec prime : int -> bool
= fun n ->if n<2 then false
	        else if (n=2) then true
					else sieve(n,2);; 
