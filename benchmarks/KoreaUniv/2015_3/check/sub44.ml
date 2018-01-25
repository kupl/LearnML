type exp = V of var
           | P of var * exp
           | C of exp * exp
  and var = string

  let var s = V(s)
  let proc s t = P(s, t)
  let call t u = C(t, u)
  
  let rec free_vars = function
	V(s) -> [s]
	| P(s, t) -> List.filter (fun x -> x<>s) (free_vars t)
	| C(t, u) ->
		let f_t = free_vars t in 
		let f_u = free_vars u in
		  List.append f_t (List.filter (fun x -> not (List.mem x f_t)) f_u)
let rec fresh_var v l =
 if List.mem v l then fresh_var(v ^ "'") l
 else v

 let check : exp -> bool
 = fun e ->
 if free_vars e = [] then true
 else false
