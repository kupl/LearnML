let rec uniq : 'a list -> 'a list
= fun lst ->
  let rec remove_dup : 'a list -> 'a list -> 'a list
  = fun l1 l2 ->
    match l1 with
      | [] -> l2
      | hd1 :: tl1 ->
      let origin = l2 in
        let rec compare : 'a list -> 'a list
        = fun org ->
          match org with
            | [] -> remove_dup tl1 (l2 @ [hd1])
            | hd2 :: tl2 ->
              if hd2 = hd1 then remove_dup tl1 l2
              else compare tl2
        in compare origin
    in remove_dup lst [];;
    
