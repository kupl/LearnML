let rec remove : 'a -> 'a list -> 'a list
= fun e lst ->
  match lst with
  | [] -> []
  | hd::tl -> if (e = hd) then remove e tl else hd::(remove e tl)

let rec uniq : 'a list -> 'a list
= fun lst ->
  match lst with
  | [] -> []
  | hd::tl -> hd::(remove hd (uniq tl))
;;

