(* 2014-15113 Kim MinJI *)

let rec iter (n:int) f = 
	if n<=0 then fun x->x
	else fun x->(f ((iter (n-1) f) x))



		
