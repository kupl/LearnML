(* let app : 'a list -> 'a list -> 'a list
= fun l1 l2 -> *)

exception Empty;;

let rec last l =
  match l with
    | [] -> raise Empty
    | [a] -> a
    | _::tl -> last tl
;;

let rec append l1 l2 =
  match l1 with
    | [] -> l2
    | hd::tl -> hd::(append tl l2)
;;

let rec app : 'a list -> 'a list -> 'a list
 = fun l1 l2 ->
   match l1 with
     | [] -> l2
     | hd::tl -> if last l2 = hd then app tl l2 else append l2 l1
;; 

app [4;4;3;4;4;5;6;7] [1;2;3;4];;