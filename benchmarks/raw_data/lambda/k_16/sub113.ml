
  type lambda =
  | V of var
  | P of var * lambda
  | C of lambda * lambda
  and var = string

  let check : lambda -> bool
  = fun lambda -> let rec envcheck: lambda * lambda list -> bool = fun (lambda,li)->
		begin match lambda with 
			V x->( match li with 
				[]->false
				|h::t-> if h=V x then true else envcheck(V x,t) )
			|P(v,e)->envcheck (e, (V v)::li)
			|C(e1,e2)-> envcheck(e1,li)&&envcheck(e2,li)
		end in envcheck(lambda,[])
