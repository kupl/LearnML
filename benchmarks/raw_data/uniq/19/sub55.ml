let uniq : 'a list -> 'a list
= fun lst ->
  let rec find_a a lst =
    match lst with
    | [] -> true
    | hd::tl ->
      if a = hd
      then false
      else find_a a tl
  in
    let rec func_uniq l1 l2 =
      match l1 with
      | [] -> l2
      | hd::tl ->
        if find_a hd l2
        then func_uniq tl (l2 @ [hd])
        else func_uniq tl l2
    in func_uniq lst [];;