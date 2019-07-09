let rec has : 'a -> 'a list -> bool
= fun e lst -> match lst with
  | [] -> false
  | hd::tl ->
    if hd = e then true
    else has e tl;;

let rec app : 'a list -> 'a list -> 'a list
= fun l1 l2 -> match l1 with
  | [] -> l2
  | hd::tl ->
    if has hd l2 then app tl l2
    else app tl (l2@[hd]);;
    
(*
app [4;5;6;7] [1;2;3;4];;

app [1;2;3;4;5;6] [1;2;3;4;5];;
*)