type lambda = 
  | V of var
  | P of var * lambda
  | C of lambda * lambda
and var = string

let rec find : var list -> var -> bool
= fun l v ->
  match l with
    |[] -> false
    |h::t -> if(h=v) then true else find t v

let check : lambda -> bool
= fun lam -> 
  let s = [] in
  let rec checking : lambda -> var list -> bool
  = fun lamb stack ->
  match lamb with
    | V a -> find stack a
    | P (v,l) -> checking l (v::stack)
    | C (l1,l2) -> checking l1 stack && checking l2 stack
  in checking lam s;;
  
let good1 = P ("a", V "a")
let good2 = P ("a", P ("a", V "a"))
let good3 = P ("a", P ("b", C (V "a", V "b")))
let good4 = P ("a", C (V "a", P ("b", V "a")))

let ill1 = P ("a", V "b")
let ill2 = P ("a", C (V "a", P ("b", V "c")))
let ill3 = P ("a", P ("b", C (V "a", V "c")));;

check good1;;
check good2;;
check good3;;
check good4;;
check ill1;;
check ill2;;
check ill3;;

