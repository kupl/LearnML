let rec app : 'a list -> 'a list -> 'a list
= fun l1 l2 -> (* TODO *)
let rec check a l = 
  match l with
    |[] -> false
    |h::t -> if h = a then true else false || check a t in
  match l1 with 
    |[] -> l2
    |hd::tl -> if check hd l2 = false then app tl (l2@[hd]) else app tl l2;;