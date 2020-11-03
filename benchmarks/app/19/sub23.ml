let app : 'a list -> 'a list -> 'a list
= fun l1 l2 ->
  let rec addone ls1 ls2 =
    match ls1 with
      | [] -> ls2
      | hd::tl ->
        let rec chec lis e =
          match lis with
            | [] -> false
            | hd::tl ->
              if hd = e then true
              else chec tl e
        in if (chec l2 hd) = true then addone tl ls2
        else addone tl (ls2 @ [hd])
  in addone l1 l2;;