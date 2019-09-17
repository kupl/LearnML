
  type lambda =
  | V of var
  | P of var * lambda
  | C of lambda * lambda
  and var = string

let rec ck : lambda*string list -> int
= fun (lambda, lst)->
  

  match lambda with
  |P(va, ex) -> ck(ex, va::lst)
  |C(ex1, ex2) -> ck(ex1, lst) + ck(ex2, lst)
  |V(va) -> if (List.mem va lst = true) then 0 else 1 




(*=====================================================*)
(*스타또!*)
(*=====================================================*)
  let check : lambda -> bool
  = fun lambda -> (* TODO *)
    
    if(ck(lambda, [])=0) then true
    else false

