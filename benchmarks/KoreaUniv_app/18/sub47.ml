let rec app : 'a list -> 'a list -> 'a list
= fun l1 l2 ->
  let rec delint left right x =
    match right with
      |[] -> left
      |hd::tl -> delint (left @ (if hd = x then [] else [hd])) tl x in 
  let rec deldup left right lst =
    match right with
      |[] -> lst
      |hd::tl -> deldup (left @ [hd]) tl (delint [] lst hd) in 
  l2 @ (deldup [] l2 l1) ;;
  
app [4;5;6;7] [1;2;3;4];;
app [3;4;5;6] [1;2;3;4];;
app [2;4;5] [1;1;2;2;3;3];;