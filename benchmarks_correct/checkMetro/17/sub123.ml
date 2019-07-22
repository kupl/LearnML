(*HW2-Exercise 4*)
type metro = STATION of name
    | AREA of name * metro
    | CONNECT of metro * metro
and name = string

let checkMetro met =
    let rec checkId met id_list =
        match met with
        | STATION(name) -> List.mem name id_list
        | AREA(name, met0) -> checkId met0 (name::id_list)
        | CONNECT(met1, met2) -> ((checkId met1 id_list) && (checkId met2 id_list))
    in
    checkId met []
