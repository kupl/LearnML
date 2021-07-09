type lambda = V of var | P of (var * lambda) | C of (lambda * lambda)

and var = string

let rec __s3 (__s4 : lambda) : string list =
  match __s4 with
  | V __s5 -> [ __s5 ]
  | P (__s6, __s7) ->
      List.filter (fun (__s8 : string) -> not (__s6 = __s8)) (__s3 __s7)
  | C (__s9, __s10) -> __s3 __s9 @ __s3 __s10


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
  List.length (__s3 met) = 0
