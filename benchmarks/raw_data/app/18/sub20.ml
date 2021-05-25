let rec app : 'a list -> 'a list -> 'a list
= fun l1 l2 ->
  let rec is_not_exist  : 'a -> 'a list -> bool
  = fun v lst ->
    match lst with
      | [] -> true
      | hd::tl -> if hd = v then false else is_not_exist v tl
  in
  let rec remove_dup : 'a list -> 'a list -> 'a list -> 'a list
  = fun l1 l2 result ->
    match l1, l2 with
      | [], [] -> result
      | hd::tl, [] -> if is_not_exist hd result then remove_dup tl l2 (result @ [hd]) else remove_dup tl l2 result
      | _, hd::tl -> if is_not_exist hd result then remove_dup l1 tl (result @ [hd]) else remove_dup l1 tl result
  in
  remove_dup l1 l2 [];;