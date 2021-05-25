let rec isin a lst = 
match lst with
| [] -> false
| hd::tl -> if a=hd then true else isin a tl;;


let rec app_rec l1 l2 = 
match l1 with
| [] -> l2
| hd::tl -> if (isin hd l2) then app_rec tl l2 else app_rec tl (l2 @ [hd]);;


let app : 'a list -> 'a list -> 'a list
= fun l1 l2 -> app_rec l1 l2;;

