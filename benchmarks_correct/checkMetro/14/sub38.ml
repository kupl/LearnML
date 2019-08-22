type lambda = V of var
           | P of var * lambda
           | C of lambda * lambda
and var = string

(* Make a list and match *)
let check (m: lambda): bool =
  let rec check_list (m: lambda) (l: var list): bool =
    match m with
    | V var1 -> List.mem var1 l
    | P (var1, lambda1) -> check_list lambda1 (var1::l)
    | C (lambda1, lambda2) -> check_list lambda1 l && check_list lambda2 l
  in
  check_list m []
