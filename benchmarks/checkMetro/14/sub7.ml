type metro = STATION of name
            | AREA of name * metro
            | CONNECT of metro * metro
and name = string

let checkMetro met =
  let rec checkMetroWithNames (m, inArea) =
    match m with
      STATION n ->
        let rec findNameInAreaList (name, l) =
          match l with
            [] -> false
            | e::ll ->
              if String.compare name e == 0
                then true
              else
                findNameInAreaList (name, ll) in
       findNameInAreaList (n, inArea)
      | AREA (n, me) -> checkMetroWithNames(me, [n] @ inArea)
      | CONNECT (m1, m2) ->
        if checkMetroWithNames (m1, inArea) && checkMetroWithNames (m2, inArea)
          then true
        else
          false in
 checkMetroWithNames (met, [])
