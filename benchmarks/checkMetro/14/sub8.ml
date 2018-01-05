type metro = STATION of name
| AREA of name * metro
| CONNECT of metro * metro
and name = string;;

let rec checkMetroHelper areaList metro =
    match metro with
    | STATION name ->
        if List.mem name areaList
            then true
            else false
    | AREA (name, metro) -> checkMetroHelper (name::areaList) metro
    | CONNECT (metro1, metro2) -> checkMetroHelper areaList metro1 && checkMetroHelper areaList metro2;;

let rec checkMetro metro =
    checkMetroHelper ([]) metro;;

(*
let print_bool x = print_endline (string_of_bool x);;

print_bool(checkMetro ( CONNECT (AREA ("a", STATION "a"), AREA ("b", AREA("a", CONNECT(STATION "b", STATION "a"))))));;
print_bool(checkMetro ( CONNECT (AREA ("c", STATION "c"), AREA ("b", AREA("a", CONNECT(STATION "b", STATION "c"))))));;
*)
