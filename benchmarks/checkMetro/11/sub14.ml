
(* 2008-11720 Á¶°Ü¸® *)

type metro = STATION of name
        | AREA of name * metro
	    | CONNECT of metro * metro
and name = string

let checkMetro met =

let rec checklist met l =
    match met with
	    STATION s -> List.mem s l
	    | AREA (n, m) -> (checklist m (n::l))
	    | CONNECT (m1, m2) -> (checklist m1 l) && (checklist m2 l)
in
(checklist met [])


