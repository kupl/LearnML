type metro = STATION of name
| AREA of name * metro
| CONNECT of metro * metro
and name = string

let startlist = []

let rec checkMetro1 m listofmetro= 
  match m with
  | STATION name1 -> List.mem name1 listofmetro
  | AREA(n1, metro) -> checkMetro1 metro (n1::listofmetro)
  | CONNECT(metro1, metro2) -> (checkMetro1 metro1 listofmetro)&&(checkMetro1 metro2 listofmetro)

let checkMetro m = checkMetro1 m startlist
