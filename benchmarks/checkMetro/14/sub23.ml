type metro = STATION of name
| AREA of name * metro
| CONNECT of metro * metro
and name = string

let checkMetro m =

  let rec isTrue m l =
    match m with
    | STATION n -> if (List.mem n l) then true else false 
    | AREA (n, mm) -> 
        if (List.mem n l) then (isTrue mm l)
        else (isTrue mm ([n]@l))
    | CONNECT (mm1, mm2) ->
        (isTrue mm1 l) && (isTrue mm2 l)
    in

  match m with
  | STATION n -> false
  | AREA (n, mm) -> (isTrue mm [n])
  | CONNECT (mm1, mm2) -> (isTrue mm1 []) && (isTrue mm2 [])
