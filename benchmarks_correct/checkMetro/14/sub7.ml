type lambda = V of var
            | P of var * lambda
            | C of lambda * lambda
and var = string

let check met =
  let rec checkWithNames (m, inArea) =
    match m with
      V n ->
        let rec findNameInAreaList (var, l) =
          match l with
            [] -> false
            | e::ll ->
              if var=e
                then true
              else
                findNameInAreaList (var, ll) in
       findNameInAreaList (n, inArea)
      | P (n, me) -> checkWithNames(me, [n] @ inArea)
      | C (m1, m2) ->
        if checkWithNames (m1, inArea) && checkWithNames (m2, inArea)
          then true
        else
          false in
 checkWithNames (met, [])
