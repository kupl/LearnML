(*true -> not included in the list*)
let rec check : 'a -> 'a list -> bool
= fun a l ->
  match l with
  | [] -> true
  | hd::tl -> if a=hd then false else check a tl;;

let rec app : 'a list -> 'a list -> 'a list
= fun l1 l2 -> 
  match l1 with
  | [] -> l2
  | hd::tl -> if check hd l2 then app tl (l2@[hd]) else app tl l2;;
  
(*
app [4;5;6;7] [1;2;3;4];;
app [1;3;5;7;9] [4;5;6];;
*)