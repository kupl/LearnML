type metro = STATION of name
            |AREA of name * metro
            |CONNECT of metro * metro
and name = string

let rec check var_metro = 
  match var_metro with
    CONNECT (m1, m2) -> (check m1)@(check m2)
    |AREA (n,m) ->  (List.filter (fun x -> ((String.compare x n) != 0)) (check m))
    |STATION n -> n::[]
let checkMetro var_metro = (List.length (check var_metro)) == 0
