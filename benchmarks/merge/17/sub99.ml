let rec merge : int list * int list -> int list = fun (a, b) ->
  match a with
  | [] -> b
  | hda :: tla -> (
    match b with
    | [] -> a
    | hdb :: tlb ->
      if (hda > hdb)
        then hda :: (merge (tla, b))
        else hdb :: (merge (a, tlb))
    )
