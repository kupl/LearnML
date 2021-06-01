type lambda = V of var
         | P of var * lambda
         | C of lambda * lambda
and var = string

let rec isinList a l = 
  match l with
      [] -> false
    | h::t -> if(h = a) then true else isinList a t
;;
let rec validCheck : lambda -> var list -> bool
  = fun e l ->
    match e with
        V(_v) -> isinList _v l
      | P(_v, _e) -> let _l = _v::l in validCheck _e _l
      | C(_e1, _e2) -> validCheck _e1 l && validCheck _e2 l
;;
let check : lambda -> bool
  = fun e -> validCheck e [];;
