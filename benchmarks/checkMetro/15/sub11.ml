
type metro = STATION of name
            | AREA of name * metro
            | CONNECT of metro * metro
        and name = string

let checkMetro: metro -> bool = fun(m) ->
    let rec checkMetroInternal: string list * metro -> bool = fun(sl, m) ->
        match m with
        | STATION(n) -> List.mem n sl
        | AREA(n, m) -> checkMetroInternal(n::sl, m)
        | CONNECT(m1, m2) -> checkMetroInternal(sl, m1) && checkMetroInternal(sl, m2)
        in
    checkMetroInternal([], m)


(*

let t1 = AREA("a", STATION "a")
let t2 = AREA("a", AREA("a", STATION "a"))
let t3 = AREA("a", AREA("b", CONNECT(STATION "a", STATION "b")))
let t4 = AREA("a", CONNECT(STATION "a", AREA("b", STATION "a")))

let f1 = AREA("a", STATION "b")
let f2 = AREA("a", CONNECT(STATION "a", AREA("b", STATION "c")))
let f3 = AREA("a", AREA("b", CONNECT(STATION "a", STATION "c")))

let _ = print_endline(string_of_bool(checkMetro(t1)))
let _ = print_endline(string_of_bool(checkMetro(t2)))
let _ = print_endline(string_of_bool(checkMetro(t3)))
let _ = print_endline(string_of_bool(checkMetro(t4)))
let _ = print_endline(string_of_bool(checkMetro(f1)))
let _ = print_endline(string_of_bool(checkMetro(f2)))
let _ = print_endline(string_of_bool(checkMetro(f3)))
*)
