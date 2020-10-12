
  type lambda =
  | V of var
  | P of var * lambda
  | C of lambda * lambda
  and var = string
    
  let rec checklist : (var list)*(var) -> bool
  = fun (l,v) -> match l with
  |[] -> false
  |hd::tl -> if hd=v then true else checklist (tl,v)

  let rec check2 : lambda*(var list) -> bool
  = fun (lambda,l) -> match lambda with
  |V v -> if (checklist(l,v)) then true else false
  |P (v,e) -> check2 (e,v::l)
  |C (e1,e2) -> check2(e1,l)&&check2 (e2,l) 


  let check : lambda -> bool
  = fun lambda -> check2 (lambda,[]) 