let rec app : 'a list -> 'a list -> 'a list
= fun l1 l2 -> match l1 with
  | [] -> l2
  | hd::tl -> if (search hd l2) then app tl l2 else app tl (l2@[hd])
  
and search : 'a -> 'a list -> bool
= fun s l -> match l with
  | [] -> false
  | hd::tl -> if s = hd then true else search s tl;;
  
app [4;5;6;7] [1;2;3;4];;