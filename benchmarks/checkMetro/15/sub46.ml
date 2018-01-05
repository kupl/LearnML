type metro =
    STATION of name
    | AREA of name * metro
    | CONNECT of metro * metro
and name = string

let checkMetro metro =

    let rec metroWithList metro mlist =
        match metro with
        |STATION n -> (List.mem n mlist)
        |AREA (n,m) -> (metroWithList m ((n::[])@mlist))
        |CONNECT (m1,m2) -> (metroWithList m1 mlist) && (metroWithList m2 mlist) in
    
    metroWithList metro []

