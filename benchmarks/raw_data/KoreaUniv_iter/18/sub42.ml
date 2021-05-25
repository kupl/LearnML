let rec iter : int * (int -> int) -> (int -> int)
= fun (n,f) -> if n = 0
		then
		  fun (x:int) -> x
		else
		   fun (x:int) -> f(iter((n-1), f) x)
;;

(*iter(2, fun x -> 2+x) 0;;*)

