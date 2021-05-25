let rec uniq : 'a list -> 'a list
= fun lst -> 
  let rec delint left right x =
    match right with
      |[] -> left
      |hd::tl -> delint (left @ (if hd = x then [] else [hd])) tl x in 
  let rec deldup left right =
    match right with
      |[] -> left
      |hd::tl -> deldup (left @ [hd]) (delint [] tl hd) in 
  deldup [] lst;;


  
uniq [5;6;5;4];;
uniq [1;2;3;4;2;3;1;4;2;3;4;1];;