type var = string

type lambda = V of var
    | P of var * lambda
    | C of lambda * lambda


(*let rec getan m : var list =
    match m with
    | P(a, b) -> a::[] @ (getan b)
    | C(a, b) -> (getan a) @ (getan b)
    | _ -> []*)

(*let rec getan m : var list = 
    match m with
    | P (a, b) -> l @ a::[]
    | C (a, b) -> l @ (getan a) @ (getan b)
    | _ -> []*)

(*let rec getsn m : var list = 
    match m with
    | V a -> a::[]
    | P(a, b) -> getsn b
    | C(a, b) -> (getsn a) @ (getsn b)*)

(*let rec check m : bool = 
  (*  let memm sm : bool = (List.mem sm (getan m)) in*)
        match m with
        | P(a, V b) -> (List.for_all memm (getsn m))
        | P(a, b) -> (List.for_all memm (getsn m)) && (check b)
        | C(V a, V b) -> false
        | C(V a, b) -> (check b)
        | C(a, V b) -> (check a)
        | C(a, b) -> (check a) && (check b)
        | _ -> List.for_all memm (getsn m)*)

let rec varFilter m l : bool =
    match m with
    | P(a, b) -> (varFilter b (a::l))
    | C(a, b) -> (varFilter a l) && (varFilter b l)
    | V a -> (List.mem a l)

let rec check m : bool = 
    match m with
    | P(a, b) -> varFilter m []
    | C (a, b) -> (check a) && (check b)
    | _ -> false
