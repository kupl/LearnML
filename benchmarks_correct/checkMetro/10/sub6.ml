type metro = STATION of name
| AREA of name * metro
| CONNECT of metro * metro
and name = string
;;

let rec check(l, m) =
        match m with
        STATION a -> List.exists (fun n-> n=a) l
        |AREA(a, b) -> check(a::l, b)
        |CONNECT(a, b) -> check(l, a) && check(l, b)
;;

let checkMetro m =
        check([], m)
;;
