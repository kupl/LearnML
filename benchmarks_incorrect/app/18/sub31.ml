let rec app : 'a list -> 'a list -> 'a list
= fun l1 l2 ->
  match l1 with
    |[] -> l2
    |head::tail -> 
      let rec is_dup lst x =
        match lst with
          |[] -> false
          |head2::tail2 -> 
            if x = head2 then true
            else is_dup tail2 x 
            in if is_dup l2 head = true then app tail l2
                else app tail (l2 @ [head]);;
                
