let app : 'a list -> 'a list -> 'a list
= fun l1 l2 ->
  match l1 with
    | [] -> l2;;
    | hd :: tl -> if hd = in l2 then app l1 l2;;
    else then app (l1 :: hd) tl;;
