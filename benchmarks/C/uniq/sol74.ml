let rec uniq : 'a list -> 'a list
= fun lst ->
  
  let rec removeDuplicate l n =
    match l with
      | [] -> []
      | hd::tl -> 
        if hd = n then removeDuplicate tl n 
        else [hd]@(removeDuplicate tl n)
    in
  
  match lst with
    | [] -> []
    | hd::tl -> 
      [hd]@(uniq (removeDuplicate tl hd))
  ;;
