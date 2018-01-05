type metro = 
  | STATION of name
  | AREA of name * metro
  | CONNECT of metro * metro
and 
  name = string

let rec checkMetro (m: metro) : bool =
  match m with
  | STATION n -> true
  | CONNECT (m1, m2) -> (checkMetro m1) && (checkMetro m2)
  | AREA (n, m) -> 
      ( match m with
      | STATION n2 -> (if (n=n2) then true else false)
      | _ -> (checkMetro m)
      )


