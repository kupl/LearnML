let rec in_it = fun l1 x ->
  match l1 with
    |[] -> false
    |_-> if List.hd l1 = x then true else in_it (List.tl l1) x;;

let rec app_help : 'a list -> 'a list -> 'a list
= fun l1 l2 ->
  match l1 with 
    |hd::tl -> if in_it l2 (List.hd l1) = false then List.hd l1::app_help (List.tl l1) l2 else app_help (List.tl l1) l2 
    |[] -> [];;

let rec app : 'a list -> 'a list -> 'a list
=fun l1 l2 ->
  l2 @ (app_help l1 l2);;
    
app [4;5;6;7] [1;2;3;4];;