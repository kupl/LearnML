let rec check_dup a l =
  match l with
    |[] -> true
    |hd::tl ->
      if a = hd then false
      else check_dup a tl;;

let rec app : 'a list -> 'a list -> 'a list
= fun l1 l2 -> (* TODO *)
  match l1 with
    |[] -> l2
    |hd::tl -> 
      if check_dup hd l2 = true
      then hd::(app tl l2)
      else app tl l2;;