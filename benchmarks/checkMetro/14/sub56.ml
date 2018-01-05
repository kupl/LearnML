type metro =
    | STATION of name
    | AREA of name * metro
    | CONNECT of metro * metro
and name = string;;

let rec checkMetro metro =
    let rec checkMetroWithNames metro names =
        match metro with
        | STATION name -> List.exists (fun x -> x = name) names
        | AREA (name, metro) -> checkMetroWithNames metro (name::names)
        | CONNECT (metro0, metro1) -> (checkMetroWithNames metro0 names) &&
        (checkMetroWithNames metro1 names)
    in
    checkMetroWithNames metro [];;
