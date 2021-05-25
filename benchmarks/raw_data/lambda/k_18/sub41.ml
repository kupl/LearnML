type lambda =
    | V of var
    | P of var * lambda
    | C of lambda * lambda
and var = string

let check : lambda -> bool
= fun lam ->
    let rec find v lst =
        match lst with
        | x :: xs -> if x = v then true else find v xs
        | [] -> false in
    let rec check' vlst expr =
        match expr with
        | V v -> find v vlst
        | P (v, b) -> check' (v :: vlst) b
        | C (f, a) -> check' vlst f && check' vlst a in
    check' [] lam
;;

check (P ("a", V "a"));;
check (P ("a", P ("a", V "a")));;
check (P ("a", P ("b", C (V "a", V "b"))));;
check (P ("a", C (V "a", P ("b", V "a"))));;

check (P ("a", V "b"));;
check (P ("a", C (V "a", P ("b", V "c"))));;
check (P ("a", P ("b", C (V "a", V "c"))));;
