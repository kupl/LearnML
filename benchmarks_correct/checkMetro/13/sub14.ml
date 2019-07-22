type metro = STATION of name
    | AREA of name * metro
    | CONNECT of metro * metro
and name = string

let checkMetro metro = 
    let rec rec_check m lst =
        match m with
        | STATION name -> List.mem name lst
        | AREA (name, m1) -> rec_check m1 (name::lst)
        | CONNECT (m1, m2) -> (rec_check m1 lst) && (rec_check m2 lst)
    in rec_check metro []
       
