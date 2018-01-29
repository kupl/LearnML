
  type exp =
  | V of var
  | P of var * exp
  | C of exp * exp
  and var = string

  let check : exp -> bool
  = fun exp -> let rec envcheck: exp * exp list -> bool = fun (exp,li)->
		begin match exp with 
			V x->( match li with 
				[]->false
				|h::t-> if h=V x then true else envcheck(V x,t) )
			|P(v,e)->envcheck (e, (V v)::li)
			|C(e1,e2)-> envcheck(e1,li)&&envcheck(e2,li)
		end in envcheck(exp,[])
