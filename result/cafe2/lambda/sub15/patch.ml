type lambda = V of var | P of (var * lambda) | C of (lambda * lambda)

and var = string

let check (m : lambda) : bool =
  let rec areaList (m1 : lambda) : string list =
    match m1 with P (a, b) -> a :: areaList b | _ -> []
  in

  let rec stationList (m2 : lambda) : string list =
    match m2 with
    | V a -> [ a ]
    | P (a, b) -> List.filter (fun (__s8 : string) -> __s8 != a) (stationList b)
    | C (a, b) -> stationList a @ stationList b
  in

  let rec searchArea (al : string list) (st : string) : bool =
    match al with
    | [] -> false
    | hd :: tl -> if hd = st then true else searchArea tl st
  in

  let rec matching (al : string list) (sl : string list) : bool =
    match sl with
    | [] -> true
    | hd :: tl -> if searchArea al hd = false then false else matching al tl
  in

  let (_ : string list) = areaList m in

  let (_ : string list) = stationList m in
  matching (areaList m) (stationList m)
