type lambda = 
  | V of var
  | P of var * lambda
  | C of lambda * lambda
and var = string


type t = var list
let empty = []

let push : var -> t -> t
= fun v stack ->
  v::stack
  
let rec lookup_stack : var -> t -> bool
= fun v stack ->
  match stack with
    | [] -> false
    | hd::tl -> if v=hd then true else lookup_stack v tl

let check : lambda -> bool
= fun lam -> 
  let rec find_v : lambda -> t -> bool
  = fun lam stack ->
    match lam with
      | V (v) -> lookup_stack v stack
      | P (v, l) -> 
        (let stack' = push v stack in
        find_v l stack')
      | C (l1, l2) -> (find_v l1 stack)&&(find_v l2 stack)
  in
  find_v lam empty;;
