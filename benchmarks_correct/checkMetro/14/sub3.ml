(* C:\OCaml\lib\CheckMertoMap.ml *)

type var = string

type lambda = V of var
  |P of var * lambda
  |C of lambda * lambda

let rec check_list (lst, var) = 
  match lst with
  |[] -> false
  |head::tail ->
      (if head = var then true
	  else (check_list (tail, var)))
  
let rec list_make (lst, lambda) =
  match lambda with
  |V a -> check_list (lst, a)
  |P (a, b) -> list_make (a::lst, b)
  |C (a, b) -> list_make (lst, a) && list_make (lst, b)

let rec check lambda =
  match lambda with
  |V a -> false
  |P (a, b) -> list_make(a::[], b)
  |C (a, b) -> (check a) && (check b)
      
