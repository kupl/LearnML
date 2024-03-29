type lambda = V of var
					|P of var * lambda
					|C of lambda * lambda
and var = string

let eval mlist = 
	let rec ival k one_list	=
		 match k with
			|V a -> List.mem a one_list
			|P (a,b)-> ival b(a::one_list)
			|C (a,b) ->ival a one_list && ival b one_list in ival mlist[]

let check : lambda -> bool
= fun e -> if (eval e=true)  then true else false
;;
