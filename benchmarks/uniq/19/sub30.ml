let uniq : 'a list -> 'a list
= fun lst -> 
  match lst with
    | [] -> [];;
    | hd :: tl -> if hd = in tl then uniq tl;;
    else then hd :: (uniq tl);;
