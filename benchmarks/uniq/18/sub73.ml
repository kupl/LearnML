let rec remove_elem : 'a -> 'a list -> 'a list
= fun e lst ->
  match lst with
  | [] -> []
  | hd::tl -> if e = hd then remove_elem e tl else hd::(remove_elem e tl)
  
let rec uniq : 'a list -> 'a list
= fun lst ->
  match lst with
  | [] -> []
  | hd::tl -> hd::(remove_elem hd (uniq tl))
;;
