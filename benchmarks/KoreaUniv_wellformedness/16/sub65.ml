
  type exp =
  | V of var
  | P of var * exp
  | C of exp * exp
  and var = string

let rec ck : exp*string list -> int
= fun (exp, lst)->
  

  match exp with
  |P(va, ex) -> ck(ex, va::lst)
  |C(ex1, ex2) -> ck(ex1, lst) + ck(ex2, lst)
  |V(va) -> if (List.mem va lst = true) then 0 else 1 




(*=====================================================*)
(*스타또!*)
(*=====================================================*)
  let check : exp -> bool
  = fun exp -> (* TODO *)
    
    if(ck(exp, [])=0) then true
    else false

