(* 4190.310 Programming Languages - Daegeun Lee <elnn@elnn.kr> *)

type metro = STATION of name
           | AREA of name * metro
           | CONNECT of metro * metro
and name = string

let checkMetro = fun metro ->
    let rec contains = fun (lst, x) ->
        match lst with
        | [] -> false
        | h::t -> if h = x then true
                  else contains(t, x)
    in
    let rec check = fun (metro, environ) ->
        match metro with
        | STATION name -> contains(environ, name)
        | AREA (name, metro) -> check(metro, name::environ)
        | CONNECT (m1, m2) -> check(m1, environ) && check(m2, environ)
    in
    check(metro, [])

