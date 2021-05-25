let rec remove_ele : 'a -> 'a list -> 'a list
= fun ele lst -> match lst with
  | [] -> lst
  | hd::tl ->
    if ele = hd then remove_ele ele tl
    else hd :: remove_ele ele tl;;

let rec remove_all : 'a list -> 'a list -> 'a list
= fun list1 list2 -> match list1 with
  | [] -> list2
  | hd::tl -> remove_all tl (remove_ele hd list2);;

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

let app : 'a list -> 'a list -> 'a list
= fun l1 l2 -> uniq(l2 @ (remove_all l2 l1));;