let rec dupck : 'a list -> 'a -> bool
= fun lst n -> match lst with
  | [] -> false
  | hd::tl -> if hd=n then true else dupck tl n;;

let rec app : 'a list -> 'a list -> 'a list
= fun l1 l2 -> match l1 with
  | [] -> l2
  | hd::tl -> if dupck l2 hd = false then app tl (l2@[hd]) else app tl l2;;
  
app [4;5;6;7] [1;2;3;4];;
