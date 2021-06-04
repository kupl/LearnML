type lambda = V of var | P of (var * lambda) | C of (lambda * lambda)

and var = string

let __s3 ((__s4 : string list), (__s5 : string)) : bool =
  if List.exists (fun (__s19 : string) -> __s19 = __s5) __s4 then true
  else false


let rec __s6 ((__s7 : string list), (__s8 : lambda)) : bool =
  match __s8 with
  | V __s14 -> __s3 (__s7, __s14)
  | P (__s15, __s16) -> __s6 (__s7 @ [ __s15 ], __s16)
  | C (__s17, __s18) -> __s6 (__s7, __s17) && __s6 (__s7, __s18)


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
  | P (a, b) -> __s6 ([ a ], met)
  | C (a, b) -> check a && check b
  | V a -> false
