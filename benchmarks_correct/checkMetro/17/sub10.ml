(* Homework 2 - Exercise 4
 * 2011-10492 Jaeyeong Yang *)
type lambda = V of var
           | P of var * lambda
           | C of lambda * lambda
and var = string

let check: lambda -> bool = fun m ->
  let rec auc: lambda * var list -> bool = fun (mm, env) ->
    match mm with
    | V n -> List.mem n env
    | P (n, ml) -> auc (ml, n :: env)
    | C (m1, m2) -> auc (m1, env) && auc (m2, env)
  in
  auc (m, [])
