let rec uniq : 'a list -> 'a list
= fun lst -> let rec check_element el lis =
    match lis with
      [] -> false
      |hd::tl -> if hd = el then true else check_element el tl
      in let rec remove_duplicate result lis =
        match lis with
          [] -> result
          | hd::tl -> if check_element hd result then remove_duplicate result tl else remove_duplicate (result@[hd]) tl
          in remove_duplicate [] lst;;
          
uniq [5;6;6;6;6;6;6;6;6;6;6;5;4];;