let uniq : 'a list -> 'a list
= fun lst ->
  let rec varify l1 l2 =
    match l1 with
      | [] -> l2
      | hd::tl ->
        let rec chec ol e =
          match ol with
            | [] -> false
            | hd::tl ->
              if hd = e then true
              else chec tl e
        in if (chec l2 hd) = true then varify tl l2
        else varify tl (l2 @ [hd])
  in varify lst [];;