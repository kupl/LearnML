let rec uniq : 'a list -> 'a list
  = fun lst -> let rec remove : 'a -> 'a list -> 'a list
    = fun el lst -> match lst with
      | [] -> []
      | hd :: tl -> if hd = el then remove el tl else hd :: remove el tl
        in match lst with
          | [] -> []
          | hd :: tl -> hd :: uniq (remove hd tl);;
