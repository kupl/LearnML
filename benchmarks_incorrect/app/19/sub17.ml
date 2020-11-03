let rec app : 'a list -> 'a list -> 'a list
= fun l1 l2 -> 
  match l1 with
    [] -> l2
    | h::t ->
      let rec find h l2 =
        match l2 with
          [] -> false
          | head::tail ->
            if head = h then true
            else find h tail
      in
      if find h l2 = true then app t l2
      else app t (List.append l2 [h])
;;
