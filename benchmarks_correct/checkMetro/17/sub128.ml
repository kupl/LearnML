type metro = STATION of name
| AREA of name * metro
| CONNECT of metro * metro
and name = string

let rec checkMetro: metro -> bool = fun x ->
  let rec change : (metro * string list) -> ((metro * string list) list) = fun (x , l) ->
    match (x , l) with
    | (AREA (a, m) , l) -> change (m , a :: l) 
    | (STATION a , l) -> [(STATION a , l)]
    | ((CONNECT (m1, m2)) , l) -> List.append (change (m1, l)) (change (m2, l)) in
  let eval : (metro * string list)-> bool = fun x ->
    match x with 
    | (STATION a, l) -> List.mem a l 
    | (AREA (a, m) , l) -> false
    | ((CONNECT (m1, m2)) , l)-> false in
List.for_all eval (change (x, []))