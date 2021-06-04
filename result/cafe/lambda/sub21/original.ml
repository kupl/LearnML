type lambda = V of var | P of (var * lambda) | C of (lambda * lambda)

and var = string

let rec check (m : lambda) : bool =
  let rec checkElementInList ((elem : string), (lst : string list)) : bool =
    match lst with
    | hd :: tl -> if elem = hd then true else checkElementInList (elem, tl)
    | [] -> false
  in

  let rec subset (inner_list : string list) (outer_list : string list) : bool =
    match inner_list with
    | hd :: tl ->
        if checkElementInList (hd, outer_list) then subset tl outer_list
        else false
    | [] -> true
  in

  let rec makeAreaList (m_eq : lambda) (a_lst : string list) : string list =
    match m_eq with
    | V x -> a_lst
    | P (x, y) -> makeAreaList y (x :: a_lst)
    | C (x, y) -> makeAreaList x a_lst @ makeAreaList y []
  in

  let rec makeStationList (m_eq : lambda) (s_lst : string list) : string list =
    match m_eq with
    | V x -> x :: s_lst
    | P (x, y) -> makeStationList y s_lst
    | C (x, y) -> makeStationList x s_lst @ makeStationList y []
  in
  subset (makeStationList m []) (makeAreaList m [])
