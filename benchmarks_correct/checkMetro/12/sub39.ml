type metro = STATION of name
| AREA of name * metro
| CONNECT of metro * metro 

and name = string

let rec __checkMetro (m, l) =
  match m with 
  STATION a -> if (List.exists (fun x -> (x = a)) l) then true else false
  |AREA (_name, _metro) -> __checkMetro (_metro, _name::l)
  |CONNECT (m1, m2) -> __checkMetro (m1, l) && __checkMetro (m2, l)

let checkMetro m =
  __checkMetro (m, [])
