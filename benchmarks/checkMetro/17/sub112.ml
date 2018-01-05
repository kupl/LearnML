type name = string

type metro = STATION of name
    | AREA of name * metro
    | CONNECT of metro * metro


(*let rec getan m : name list =
    match m with
    | AREA(a, b) -> a::[] @ (getan b)
    | CONNECT(a, b) -> (getan a) @ (getan b)
    | _ -> []*)

(*let rec getan m : name list = 
    match m with
    | AREA (a, b) -> l @ a::[]
    | CONNECT (a, b) -> l @ (getan a) @ (getan b)
    | _ -> []*)

(*let rec getsn m : name list = 
    match m with
    | STATION a -> a::[]
    | AREA(a, b) -> getsn b
    | CONNECT(a, b) -> (getsn a) @ (getsn b)*)

(*let rec checkMetro m : bool = 
  (*  let memm sm : bool = (List.mem sm (getan m)) in*)
        match m with
        | AREA(a, STATION b) -> (List.for_all memm (getsn m))
        | AREA(a, b) -> (List.for_all memm (getsn m)) && (checkMetro b)
        | CONNECT(STATION a, STATION b) -> false
        | CONNECT(STATION a, b) -> (checkMetro b)
        | CONNECT(a, STATION b) -> (checkMetro a)
        | CONNECT(a, b) -> (checkMetro a) && (checkMetro b)
        | _ -> List.for_all memm (getsn m)*)

let rec nameFilter m l : bool =
    match m with
    | AREA(a, b) -> (nameFilter b (a::l))
    | CONNECT(a, b) -> (nameFilter a l) && (nameFilter b l)
    | STATION a -> (List.mem a l)

let rec checkMetro m : bool = 
    match m with
    | AREA(a, b) -> nameFilter m []
    | CONNECT (a, b) -> (checkMetro a) && (checkMetro b)
    | _ -> false
