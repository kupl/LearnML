(* 2015-1478 Giyeon Kim HW 2 *)

(* Exercise 4 *)
type metro = STATION of name
           | AREA of name * metro
           | CONNECT of metro * metro
     and name = string

let checkMetro: metro -> bool = fun imetro ->
    let rec checkMetroInner: metro -> name list -> bool = fun imetro ilist ->
	    match imetro with
        | STATION lname -> List.mem lname ilist
        | AREA (lname, rmetro) -> checkMetroInner rmetro (lname::ilist)
        | CONNECT (lmetro, rmetro) -> checkMetroInner lmetro ilist && checkMetroInner rmetro ilist
    in checkMetroInner imetro []

