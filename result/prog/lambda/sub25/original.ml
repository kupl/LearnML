type lambda = V of var | P of (var * lambda) | C of (lambda * lambda)

and var = string

let rec list_station (ipt : lambda) : string list =
  match ipt with
  | V a -> [ a ]
  | P (a, m) -> list_station m
  | C (m1, m2) -> list_station m1 @ list_station m2


let rec list_area (ipt : lambda) : string list =
  match ipt with
  | V a -> []
  | P (a, m) -> a :: list_area m
  | C (m1, m2) -> list_area m1 @ list_area m2


let rec list_matching ((ipt1 : 'a list), (ipt2 : 'a list)) : bool =
  match ipt1 with
  | [] -> true
  | _ ->
      if List.mem (List.hd ipt1) ipt2 then list_matching (List.tl ipt1, ipt2)
      else false


let check (ipt : lambda) : bool =
  if
    list_matching (list_station ipt, list_area ipt)
    && list_matching (list_area ipt, list_station ipt)
  then true
  else false
