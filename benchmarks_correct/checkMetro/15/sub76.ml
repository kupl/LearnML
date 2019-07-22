type metro = STATION of name
        | AREA of name * metro
        | CONNECT of metro * metro
    and name = string

let checkMetro met =
    let rec checkInclude m l =
        match m with
        | STATION s -> List.mem s l
        | AREA (a, inner_m) -> checkInclude inner_m (a::l)
        | CONNECT (a, b) -> (checkInclude a l) && (checkInclude b l)
    in checkInclude met [];;

(*
let bool_to_string e =
    match e with
    | true -> "true"
    | false -> "false";;

print_endline( bool_to_string( checkMetro( AREA("a", STATION "a") ) ) );;
print_endline( bool_to_string( checkMetro( AREA("a", AREA("a", STATION "a") ) ) ) );;
print_endline( bool_to_string( checkMetro( AREA("a", AREA("b", CONNECT(STATION "a", STATION "b")) ) ) ) );;
print_endline( bool_to_string( checkMetro( AREA("a", CONNECT(STATION "a", AREA("b", STATION "b") ) ) ) ) );;
print_endline( bool_to_string( checkMetro( AREA("a", STATION "b") ) ) );;
print_endline( bool_to_string( checkMetro( AREA("a", CONNECT(STATION "a", AREA("b", STATION "c"))) ) ) );;
print_endline( bool_to_string( checkMetro( AREA("a", AREA("b", CONNECT(STATION "a", STATION "c"))) ) ) );;
*)
