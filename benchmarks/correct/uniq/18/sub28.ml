let rec uniq : 'a list -> 'a list
= fun lst -> 
  let rec isNotIn tlst c =
    match tlst with
      | [] -> true
      | hd::tl -> 
        if hd == c then false 
        else true && (isNotIn tl c) in
    
    let rec uniqSave l1 l2 =
      match l1 with
        | [] -> l2
        | hd::tl -> 
          if isNotIn l2 hd then uniqSave tl (l2 @ [hd])
          else uniqSave tl l2 in uniqSave lst [];;


