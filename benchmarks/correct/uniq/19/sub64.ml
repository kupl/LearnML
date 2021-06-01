let rec isin a lst = 
match lst with
| [] -> false
| hd::tl -> if a=hd then true else isin a tl;;


let rec uniq_rec l1 l2 = 
match l1 with 
| [] -> l2
| hd :: tl -> if (isin hd l2) then uniq_rec tl l2 else uniq_rec tl (l2 @ [hd]) ;;



(*main*)
let uniq : 'a list -> 'a list
= fun lst -> uniq_rec lst [];;
