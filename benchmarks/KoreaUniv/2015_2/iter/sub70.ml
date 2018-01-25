let rec iter : int * (int ->int) -> (int->int)
=fun (n,f)->
			match n with
			|0-> fun x->x
			|1-> f
			|_-> let k  = iter (n-1,f) in (fun x-> k (f x))
;;
