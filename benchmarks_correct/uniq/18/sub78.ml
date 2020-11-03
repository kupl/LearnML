let rec uniq : 'a list -> 'a list
= fun lst ->
  let rec putIn list1 list2 = match list1 with
    | [] -> list2
    | hd :: tl ->
      let rec check item l = match l with
        | [] -> [item]
        | hd :: tl -> if hd = item then l else hd :: check item tl
      in putIn tl (check hd list2)
  in putIn lst [];;