type metro = STATION of name
           | AREA of name * metro
           | CONNECT of metro * metro
and name = string

let rec subCheckMetro areaList mtr = 
        match mtr with
        | STATION name -> (List.mem name areaList)
        | AREA (name, metro) -> (subCheckMetro (areaList @ [name]) metro)
        | CONNECT (m1, m2) -> (subCheckMetro areaList m1) && (subCheckMetro
        areaList m2)

let rec checkMetro mtr = 
        match mtr with
        | STATION _ -> false
        | CONNECT (a, b) -> (checkMetro a) && (checkMetro b)
        | AREA (name, metro) -> (subCheckMetro [name] metro)