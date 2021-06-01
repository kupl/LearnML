type lambda = 
  | V of var
  | P of var * lambda
  | C of lambda * lambda
and var = string

let check : lambda -> bool
= fun lam ->
  let myvarlst = [] and myfreevarlst = []
  in 
  
  let rec check2 lambda = 
    match lambda with
      | V v -> myfreevarlst@[v]
      | P (v,l) -> let dummy1 = myvarlst@[v] in check2 l
      | C (l1,l2) -> let dummy2 = check2 l1 in check2 l2
  in
  
  let rec check3 varlst freevarlst =
    match freevarlst with
      | [] -> true
      | var::tl ->
        begin
        match varlst with
          | [] -> false
          | var2::tl2 ->
            if var=var2 then check3 tl freevarlst
            else check3 varlst tl2
        end
  in
  
  let dummy = check2 lam
  in
  check3 myvarlst myfreevarlst;; 