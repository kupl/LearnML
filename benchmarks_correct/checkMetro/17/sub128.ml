type lambda = V of var
| P of var * lambda
| C of lambda * lambda
and var = string

let rec check: lambda -> bool = fun x ->
  let rec change : (lambda * string list) -> ((lambda * string list) list) = fun (x , l) ->
    match (x , l) with
    | (P (a, m) , l) -> change (m , a :: l) 
    | (V a , l) -> [(V a , l)]
    | ((C (m1, m2)) , l) -> List.append (change (m1, l)) (change (m2, l)) in
  let eval : (lambda * string list)-> bool = fun x ->
    match x with 
    | (V a, l) -> List.mem a l 
    | (P (a, m) , l) -> false
    | ((C (m1, m2)) , l)-> false in
List.for_all eval (change (x, []))