type lambda = V of var | P of (var * lambda) | C of (lambda * lambda)

and var = string

let rec check : lambda -> bool =
 fun e ->
  let rec checkp (va, ex) =
    match ex with
    | V va2 -> if va = va2 then true else false
    | P (va2, ex2) -> (
        match ex2 with
        | C (ex3, ex4) ->
            if
              (checkp (va, ex3) || checkp (va2, ex3))
              && (checkp (va, ex4) || checkp (va2, ex4))
            then true
            else false
        | _ -> if checkp (va, ex2) || checkp (va2, ex2) then true else false )
    | C (ex2, ex3) ->
        if checkp (va, ex2) && checkp (va, ex3) then true else false
  in

  match e with
  | V va -> false
  | P (va, ex) -> checkp (va, ex)
  | C (ex, ex2) -> if check ex && check ex2 then true else false
