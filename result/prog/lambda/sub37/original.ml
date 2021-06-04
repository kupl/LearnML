type lambda = V of var | P of (var * lambda) | C of (lambda * lambda)

and var = string

let rec check (met : lambda) : bool =
  let rec stationvar (f : lambda) : var list =
    match f with
    | P (a, b) -> stationvar b
    | V a -> [ a ]
    | C (a, b) -> List.append (stationvar a) (stationvar b)
  in

  let rec areavar (f : lambda) : var list =
    match f with
    | P (a, b) -> List.append [ a ] (areavar b)
    | C (a, b) -> List.append (areavar a) (areavar b)
    | V a -> []
  in

  let rec haveit (a : string list) (b : string list) : bool =
    let predicate (f : string) : bool = List.mem f b in
    List.for_all predicate a
  in

  match met with
  | P (a, b) -> haveit (stationvar b) (areavar met)
  | C (a, b) -> check a && check b
  | V a -> false
