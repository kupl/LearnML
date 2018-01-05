type metro = STATION of name
					 | AREA of name * metro
					 | CONNECT of metro * metro
and name = string

let rec checkMetro m =  
    let rec checkArea a =
           match a with
                | (l, STATION name) -> List.mem name l
                | (l, CONNECT (m1, m2)) -> checkArea (l, m1) && checkArea(l,
                m2)
                | (l, AREA (name, metro)) -> checkArea( name::l, metro) 
    in
    match m with
        | STATION _ -> false
        | CONNECT (m1, m2) -> checkMetro m1 && checkMetro m2
        | AREA (name, submetro) -> checkArea (name::[], submetro)
