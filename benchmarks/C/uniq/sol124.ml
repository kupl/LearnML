let rec remove_ele : 'a -> 'a list -> 'a list
= fun ele lst -> match lst with
  | [] -> lst
  | hd::tl ->
    if ele = hd then remove_ele ele tl
    else hd :: remove_ele ele tl;;

let rec uniq : 'a list -> 'a list
= fun lst -> match lst with
  | [] -> []
  | hd::tl -> hd::uniq (remove_ele hd tl);;