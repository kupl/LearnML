type metro = STATION of name
                   | AREA of name*metro
                   | CONNECT of metro*metro
and name = string

let rec checkPos name area =
                match area with
                | hd::tl -> if (hd=name) then true
                            else (checkPos name tl)
                | [] -> false

let rec subfunc m area =
        match m with
				| AREA (n,msub) -> (subfunc msub (area@[n]))
				| STATION n -> (checkPos n area)
        | CONNECT (n1,n2) -> ((subfunc n1 area) && (subfunc n2 area))

let checkMetro m =
    (subfunc m [])

