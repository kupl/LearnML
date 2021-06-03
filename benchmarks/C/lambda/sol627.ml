type lambda = 
  | V of var              (* variables *)
  | P of var * lambda     (* abstraction *)
  | C of lambda * lambda  (* application *)
and var = string

let rec check : lambda -> bool = fun lam ->
  let rec loop lam l =
    match lam with
      | V v -> 
        (let rec check_list l x =
          match l with
            | [] -> false
            | hd::tl -> if hd = x then true else check_list tl x
        in check_list l v)
      | P (v, e) -> loop e (l@[v])
      | C (e1, e2) -> (loop e1 l) && (loop e2 l)
  in loop lam [];;
  
check (P ("a", V "a"));;
check (P ("a", P ("a", V "a")));;
check (P ("a", P ("b", C (V "a", V "b"))));;
check (P ("a", C (V "a", P ("b", V "a"))));;
check (P ("x", C (P ("y", V "y"), V "x")));;

check (P ("a", V "b"));;
check (P ("a", C (V "a", P ("b", V "c"))));;
check (P ("a", P ("b", C (V "a", V "c"))));;