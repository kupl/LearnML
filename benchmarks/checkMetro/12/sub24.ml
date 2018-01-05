type metro = STATION of name
           | AREA of name * metro
           | CONNECT of metro * metro
and name = string

let checkMetro met = 
  let rec proc(m, met_list) = 
    match m with
    | STATION(name) -> List.mem name met_list
    | AREA(name, metro) -> proc(metro, name::met_list)
    | CONNECT(a, b) -> proc(a, met_list) && proc(b, met_list)
  in
  proc(met, [])

  
