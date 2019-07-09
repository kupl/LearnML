let rec app : 'a list -> 'a list -> 'a list
= fun l1 l2 -> 
  let rec isNotIn lst c =
    match lst with
      | [] -> true
      | hd::tl -> 
        if hd == c then false 
        else true && (isNotIn tl c) in
        
        match l1 with
            | [] -> l2
            | hd::tl -> 
              if isNotIn l2 hd then app tl (l2 @ [hd])
              else app tl l2;;

