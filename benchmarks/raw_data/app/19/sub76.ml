let rec app : 'a list -> 'a list -> 'a list
= fun l1 l2 ->
  let rec uniq : 'a list -> 'a list
  = fun lst ->
    let rec remove_dup = fun v lst ->
      match lst with
        | [] -> []
        | hd::tl -> if hd = v then remove_dup v tl else hd::(remove_dup v tl)
    in
      match lst with
        | [] -> []
        | hd::tl -> hd::(uniq (remove_dup hd tl))
  in
    uniq (l2 @ l1)
;;
