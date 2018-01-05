type metro = STATION of name
           | AREA of name * metro
           | CONNECT of metro * metro
and name = string

let checkMetro : metro -> bool =
  fun a ->
    let rec checkM (a, lst) =
      match a with
      | STATION id -> List.exists (fun b -> id = b) lst
      | AREA (id, met) -> checkM (met, id::lst)
      | CONNECT (met1, met2) -> (checkM (met1, lst)) && (checkM (met2, lst)) in
    (checkM (a, []))
