type exp = 
  V of var 
  | P of var * exp 
  | C of exp * exp 
and var = string 


let var v = V(v)
  let proc v e = P(v, e)
  let call e1 e2 = C(e1, e2)
  
  let rec free_vars = function
   V(v) -> [v]
   | P(v, e) -> List.filter (fun x -> x<>v) (free_vars e)
   | C(e1, e2) ->
      let f_e1 = free_vars e1 in 
      let f_e2 = free_vars e2 in
        List.append f_e1 (List.filter (fun x -> not (List.mem x f_e1)) f_e2)

  let rec fresh_var v1 l =
   if List.mem v1 l then fresh_var(v1 ^ "'") l
   else v1

  let check : exp -> bool
  = fun e ->
  if free_vars e = [] then true
  else false
