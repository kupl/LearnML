let rec uniq : 'a list -> 'a list
= fun lst ->
  let rec is_not_exist : 'a -> 'a list -> bool
  = fun v elst ->
    match elst with
      | [] -> true
      | hd::tl -> if hd = v then false else is_not_exist v tl
  in
  let rec uniq_in : 'a list -> 'a list -> 'a list
  = fun tmp lstn ->
    match lstn with
      | [] -> tmp
      | hd::tl -> if is_not_exist hd tmp then uniq_in (tmp @ [hd]) tl else uniq_in tmp tl
  in
  uniq_in [] lst;;