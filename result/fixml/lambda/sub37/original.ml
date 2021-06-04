type lambda = V of var | P of (var * lambda) | C of (lambda * lambda)

and var = string

let rec check : lambda -> bool =
 fun met ->
  let rec stationvar : lambda -> var list =
   fun f ->
    match f with
    | P (a, b) -> stationvar b
    | V a -> [ a ]
    | C (a, b) -> List.append (stationvar a) (stationvar b)
  in

  let rec areavar : lambda -> var list =
   fun f ->
    match f with
    | P (a, b) -> List.append [ a ] (areavar b)
    | C (a, b) -> List.append (areavar a) (areavar b)
    | V a -> []
  in

  let rec haveit : var list -> var list -> bool =
   fun a b ->
    let predicate : var -> bool = fun f -> List.mem f b in
    List.for_all predicate a
  in

  match met with
  | P (a, b) -> haveit (stationvar b) (areavar met)
  | C (a, b) -> check a && check b
  | V a -> false
