let rec app : 'a list -> 'a list -> 'a list
  = fun l1 l2 -> let rec remove : 'a -> 'a list -> 'a list
    = fun el lst -> match lst with
      | [] -> []
      | hd :: tl -> if hd = el then remove el tl else hd :: remove el tl
        in match (l2 @ l1) with
          | [] -> []
          | hd :: tl -> hd :: app [] (remove hd tl);;
