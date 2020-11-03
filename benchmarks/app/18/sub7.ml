let rec app : 'a list -> 'a list -> 'a list
= fun l1 l2 ->
  let rec dup = 
    fun l item -> match l with
      | [] -> false
      | hd::tl -> if hd = item then true
                  else dup tl item
                  in
  match l1 with
  | [] -> l2
  | hd::tl -> if dup l2 hd then app tl l2
              else app tl (l2 @ [hd]);;

