let rec merge = (fun (x, y) ->
  match (x, y) with
  | ([], _) -> y
  | (_, []) -> x
  | _ -> (
    let xHead = List.hd x in
    let yHead = List.hd y in
    if xHead > yHead then xHead :: merge (List.tl x, y)
    else yHead :: merge (x, List.tl y)
    )
  )
