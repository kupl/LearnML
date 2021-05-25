let rec uniq : 'a list -> 'a list
= fun lst ->
  match lst with
    [] -> []
    | h::t -> 
      let rec search h t =
        match t with
          [] -> []
          | head::tail ->
            if head = h then search h tail
            else head::(search h tail)
      in
      h::uniq (search h t)
;;