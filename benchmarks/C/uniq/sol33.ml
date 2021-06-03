let rec duplicatedCheck 
= fun n l -> match l with
  |[] -> false
  |hd::tl -> if n=hd then true else duplicatedCheck n tl;;

let rec myUniq 
= fun l1 l2 -> match l1 with
  |[] -> l2
  |hd::tl -> if duplicatedCheck hd l2 then myUniq tl l2 else myUniq tl (l2@[hd]);;

let rec uniq : 'a list -> 'a list
= fun lst -> myUniq lst [] ;;

