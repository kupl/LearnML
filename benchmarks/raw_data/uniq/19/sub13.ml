let rec uniq : 'a list -> 'a list
= fun lst -> let rec fuxx n l2 =
  match l2 with
    | [] -> []
    | hd::tl -> 
      if n = hd then fuxx n tl
      else hd::(fuxx n tl) in
      
    match lst with
      | [] -> []
      |h::t -> h::uniq (fuxx h t);;
      
      
      
uniq [5;6;5;4;3;3;3;4];;