let rec check element lst =
  match lst with
    []->false
    |hd::tl->if element = hd then true else check element tl;;
    
let rec app : 'a list -> 'a list -> 'a list
= fun l1 l2 -> 
  match l1 with
    []->l2
    |hd::tl->if check hd l2 then app tl l2 else app tl (l2@[hd]);;
