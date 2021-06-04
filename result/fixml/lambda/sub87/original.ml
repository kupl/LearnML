type lambda = V of var | P of (var * lambda) | C of (lambda * lambda)

and var = string

let rec var_find lambda =
  match lambda with
  | V a -> []
  | P (a, lambda1) -> a :: var_find lambda1
  | C (lambda1, lambda2) -> var_find lambda1 @ var_find lambda2


let rec lambda_find lambda =
  match lambda with
  | V a -> [ a ]
  | P (a, lambda1) -> lambda_find lambda1
  | C (lambda1, lambda2) -> lambda_find lambda1 @ lambda_find lambda2


let rec find s l =
  match l with [] -> false | hd :: tl -> if s = hd then true else find s tl


let rec find_match l1 l2 =
  match l2 with [] -> true | hd :: tl -> find hd l1 && find_match l1 tl


let check : lambda -> bool =
 fun lambda -> find_match (var_find lambda) (lambda_find lambda)
