let rec uniq : 'a list -> 'a list
= fun lst ->
  let rec is_in_list a lst =
    match lst with
      | [] -> false
      | hd::tl -> if hd = a then true else (is_in_list a tl)
  in
  let rec remove_a a lst =
    match lst with
      | [] -> []
      | hd::tl -> if hd = a then (remove_a a tl) else hd::(remove_a a tl)
  in
  match lst with
    | [] -> []
    | hd::tl -> if is_in_list hd tl = true then uniq (hd::(remove_a hd tl)) else hd::(uniq tl);;