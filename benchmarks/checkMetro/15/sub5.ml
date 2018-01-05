(* SNU Programming Language Fall 2015
 * Homework 2 
 * Exercise 3: checkMetro
 * Written by Dongho Kang 
 *)

type metro = STATION of name
| AREA of name * metro
| CONNECT of metro * metro
and name = string
;;

let rec checkMetro: metro -> bool = fun m -> 
    let rec checkMetro_sub: metro * name list -> bool = fun (m, l) ->
        match m with 
        | STATION n             -> (List.mem n l)
        | AREA (n, m_n)         -> checkMetro_sub (m_n, n::l)
        | CONNECT (m_1, m_2)    -> checkMetro_sub (m_1, l) && checkMetro_sub (m_2, l)
    in

    checkMetro_sub (m, [])
;;

