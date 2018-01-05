let rec merge: int list * int list -> int list = fun (a, b) ->
  match (a, b) with
    | [], _ -> b
    | _, [] -> a
    | ha::ta, hb::tb -> (
      if ha > hb
        then ha :: merge (ta, b)
        else hb :: merge (a, tb)
    )
;;
