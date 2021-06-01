type lambda = V of var
                   | P of var*lambda
                   | C of lambda*lambda
and var = string

let rec checkPos var area =
                match area with
                | hd::tl -> if (hd=var) then true
                            else (checkPos var tl)
                | [] -> false

let rec subfunc m area =
        match m with
				| P (n,msub) -> (subfunc msub (area@[n]))
				| V n -> (checkPos n area)
        | C (n1,n2) -> ((subfunc n1 area) && (subfunc n2 area))

let check m =
    (subfunc m [])

