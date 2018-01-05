type metro = 
    | STATION of name
    | AREA of name * metro
    | CONNECT of metro * metro
and name = string

let checkMetro m = 
    let rec helper m area_li = match m with
        STATION n -> List.exists (fun x -> x = n) area_li
        | AREA (n, m) -> helper m (n :: area_li)
        | CONNECT (m1, m2) -> (helper m1 area_li) && (helper m2 area_li)
    in
    helper m [] 
