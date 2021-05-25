type lambda = V of var
| P of var * lambda
| C of lambda * lambda
and var = string;;

let rec checkHelper areaList lambda =
    match lambda with
    | V var ->
        if List.mem var areaList
            then true
            else false
    | P (var, lambda) -> checkHelper (var::areaList) lambda
    | C (lambda1, lambda2) -> checkHelper areaList lambda1 && checkHelper areaList lambda2;;

let rec check lambda =
    checkHelper ([]) lambda;;

(*
let print_bool x = print_endline (string_of_bool x);;

print_bool(check ( C (P ("a", V "a"), P ("b", P("a", C(V "b", V "a"))))));;
print_bool(check ( C (P ("c", V "c"), P ("b", P("a", C(V "b", V "c"))))));;
*)
