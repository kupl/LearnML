exception Error

type metro = STATION of name
| AREA of name * metro
| CONNECT of metro * metro
and name = string

let checkMetro met =
let rec checkMetro_in met lst = match met with STATION(name) -> (List.mem name lst)
| AREA (name, met2) -> checkMetro_in met2 (lst@[name])
| CONNECT (met1, met2) -> (checkMetro_in met1 lst) && (checkMetro_in met2 lst) in
checkMetro_in met []

let a = AREA("a", (STATION ("a")))
let b = AREA("a", (AREA("a", (STATION ("a")))))
let c = AREA("a", (AREA("b", (CONNECT((STATION ("a")), (STATION ("b")))))))
let d = AREA("a", (CONNECT((STATION ("a")), AREA("b", (STATION ("a"))))))

let e = AREA("a", (STATION ("b")))
let f = AREA("a", (CONNECT((STATION ("a")), AREA("b", (STATION ("c"))))))
let g = AREA("a", (AREA("b", (CONNECT((STATION ("a")), (STATION ("c")))))))
let h = STATION("b")