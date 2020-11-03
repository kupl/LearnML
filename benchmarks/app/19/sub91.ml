let rec exist : 'a -> 'a list -> bool
= fun a lst ->
  match lst with
    |[] -> false
    |hd::tl -> if hd = a then true else exist a tl;;

let rec app : 'a list -> 'a list -> 'a list
= fun l1 l2 -> 
  match l1 with
    |[] -> l2
    |hd::tl -> if exist hd l2 then app tl l2
                else app tl (l2@[hd]);;