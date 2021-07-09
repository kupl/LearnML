type lambda = V of var | P of (var * lambda) | C of (lambda * lambda)

and var = string

let rec var_find (lambda : lambda) : string list =
  match lambda with
  | V a -> []
  | P (a, lambda1) -> a :: var_find lambda1
  | C (lambda1, lambda2) -> var_find lambda1 @ var_find lambda2


let rec lambda_find (lambda : lambda) : string list =
  match lambda with
  | V a -> [ a ]
  | P (a, lambda1) ->
      List.filter (fun (__s11 : string) -> __s11 != a) (lambda_find lambda1)
  | C (lambda1, lambda2) -> lambda_find lambda1 @ lambda_find lambda2


let rec find s (l : 'a list) : bool =
  match l with [] -> false | hd :: tl -> if s = hd then true else find s tl


let rec find_match (l1 : 'b list) (l2 : 'b list) : bool =
  match l2 with [] -> true | hd :: tl -> find hd l1 && find_match l1 tl


let check (lambda : lambda) : bool = List.length (lambda_find lambda) = 0
