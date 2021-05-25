let rec app : 'a list -> 'a list -> 'a list
= fun l1 l2 -> let rec check_element el lis =
    match lis with
      [] -> false
      |hd::tl -> if hd = el then true else check_element el tl
      in let rec remove_duplicate result lis =
        match lis with
          [] -> result
          | hd::tl -> if check_element hd result then remove_duplicate result tl else remove_duplicate (result@[hd]) tl
          in remove_duplicate [] (l2@l1);;

app [4;4;4;4;4;4;4;5;6;7] [1;2;3;4];;