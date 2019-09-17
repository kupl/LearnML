type lambda = V of var
| P of var * lambda
| C of lambda * lambda
and var = string

let check m =

  let rec isTrue m l =
    match m with
    | V n -> if (List.mem n l) then true else false 
    | P (n, mm) -> 
        if (List.mem n l) then (isTrue mm l)
        else (isTrue mm ([n]@l))
    | C (mm1, mm2) ->
        (isTrue mm1 l) && (isTrue mm2 l)
    in

  match m with
  | V n -> false
  | P (n, mm) -> (isTrue mm [n])
  | C (mm1, mm2) -> (isTrue mm1 []) && (isTrue mm2 [])
