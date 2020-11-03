let rec app : 'a list -> 'a list -> 'a list
= fun l1 l2 -> let rec checker a b = match a with
  | [] -> false
  | hd::tl -> if hd = b then true else checker tl b in
  match l1 with
    | [] -> l2
    | hd::tl -> if checker l2 hd then app tl l2 else app tl (l2@[hd]);;
