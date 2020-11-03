let rec app : 'a list -> 'a list -> 'a list
= fun l1 l2 ->
    let rec duplicate n lst =
      match lst with
        | [] -> false
        | hd::tl -> if hd = n then true else duplicate n tl in
        
    match l1 with
      | [] -> l2
      | hd::tl -> 
        if duplicate hd l2 then app tl l2 
        else app tl (l2@[hd])
  ;;
    