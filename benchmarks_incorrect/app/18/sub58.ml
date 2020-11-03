let rec app : 'a list -> 'a list -> 'a list
= fun l1 l2 -> (* TODO *)
  let rec is_in_list element l= 
    match l with
      | [] -> false
      | hd::tl -> if (hd=element) then true else is_in_list element tl
  in
  match l1 with
    | [] -> l2
    | hd::tl -> 
      if (is_in_list hd l2) then (app tl l2) else (app tl (l2@[hd]));;
      
app [5;6;1;2;3;4] [1;2;3;4];;
app [4;1;2;3] [4;1;1];;