
  type exp =
  | V of var
  | P of var * exp
  | C of exp * exp
  and var = string
    
  let rec checklist : (var list)*(var) -> bool
  = fun (l,v) -> match l with
  |[] -> false
  |hd::tl -> if hd=v then true else checklist (tl,v)

  let rec check2 : exp*(var list) -> bool
  = fun (exp,l) -> match exp with
  |V v -> if (checklist(l,v)) then true else false
  |P (v,e) -> check2 (e,v::l)
  |C (e1,e2) -> check2(e1,l)&&check2 (e2,l) 


  let check : exp -> bool
  = fun exp -> check2 (exp,[]) 