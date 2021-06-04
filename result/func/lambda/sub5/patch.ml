type lambda = V of var | P of (var * lambda) | C of (lambda * lambda)

and var = string

let check (m : lambda) : bool =
  let rec make_area_list (m : lambda) : string list =
    match m with
    | V n -> []
    | P (n, m1) -> make_area_list m1
    | C (m1, m2) -> make_area_list m1 @ make_area_list m2
  in

  let rec match_list_with_station (m : lambda) (l : string list) : bool =
    match m with
    | P (n, m1) -> match_list_with_station m1 (l @ [ n ])
    | C (m1, m2) -> match_list_with_station m1 l && match_list_with_station m2 l
    | V n -> if List.exists (fun (x : string) -> x = n) l then true else false
  in
  match_list_with_station m (make_area_list m)
