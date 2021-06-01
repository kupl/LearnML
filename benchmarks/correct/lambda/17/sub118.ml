type lambda =
    | V of var
    | P of var * lambda
    | C of lambda * lambda
and var = string
and areaList = string list;;

let rec isIncluded (n: var) (al: areaList) : bool =
    match al with
    | [] -> false
    | hal :: tal -> if hal = n then true else isIncluded n tal;;

let rec scan (m: lambda) (al: areaList) : bool =
    match m with
    | V(n) -> isIncluded n al
    | P(n, mm) -> scan mm (al @ [n])
    | C(m1, m2) -> (scan m1 al) && (scan m2 al);;

let rec check (m: lambda) : bool =
    scan m [];;
