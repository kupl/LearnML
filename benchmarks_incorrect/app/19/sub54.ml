let rec checklst : 'a -> 'a list -> bool
= fun a lst -> match lst with
  |[] -> false
  |hd::tl -> if hd = a then true
            else checklst a tl;;

let rec app : 'a list -> 'a list -> 'a list
= fun l1 l2 -> match l1 with
  |[] -> l2
  |hd::tl -> if checklst hd l2 = false then app tl (l2@[hd])
            else app tl l2;;

app [4;5;6;7] [1;2;3;4];;


