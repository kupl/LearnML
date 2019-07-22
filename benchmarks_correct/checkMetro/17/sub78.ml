type metro = STATION of name
           | AREA of name*metro
           | CONNECT of metro * metro
and name = string
;;

let checkMetro (m : metro) : bool = 
        let rec checkMetroRec ((m : metro), (a : string list)) : bool = 
                match m with
                | STATION n ->
                                (if List.exists (fun x -> x = n) a then true
                                else false)
                | AREA(n, m2) -> checkMetroRec(m2, n::a)
                | CONNECT(m1, m2) -> checkMetroRec(m1, a) && checkMetroRec(m2, a)
        in
        checkMetroRec (m, [])
;;
