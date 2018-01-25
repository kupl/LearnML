  type exp = V of var
           | P of var * exp
           | C of exp * exp
  and var = string

  
  let check : exp -> bool
  =fun e ->
   let rec checker fv bound_list =
      match fv with
      |V a -> List.mem a bound_list
      |P (a, b) -> checker b (a::bound_list)
      |C (a, b) -> checker a bound_list && checker b bound_list
   in
   checker e []

