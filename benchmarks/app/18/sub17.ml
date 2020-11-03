let rec app : 'a list -> 'a list -> 'a list
= fun l1 l2 -> 
  let rec iselemin e li =
    match li with
    | [] -> false
    | hd::tl -> 
      if hd = e then true
      else iselemin e tl
  in
  
  let rec uniq : 'a list -> 'a list
  = fun lst -> 
    let rec iselemin e li =
      match li with
      | [] -> false
      | hd::tl -> 
        if hd = e then true
        else iselemin e tl
    in
  
    let rec repeat l1 l2 =
      match l1 with
      | [] -> l2
      | hd::tl -> 
        if (iselemin hd l2) = true then repeat tl l2
        else repeat tl (l2 @ [hd])
    in
  
    match lst with
      | [] -> []
      | hd::tl -> repeat lst []
  in
  
  
  match l1, l2 with
  | [], [] -> []
  | [], hd::tl | hd::tl, [] -> uniq (l2 @ l1)
  | hd::tl, hd2::tl2 -> uniq (l2 @ l1);;
    (*if iselemin hd l2 = false then app tl (l2 @ [hd])
    else app tl l2;;*)
    
app [4;5;6;7] [1;2;3;4];;
app [1;2;3] [4;5;1;2;6;7];;
app [1;2;3] [1;1;1;3;4;5];;