let rec determine n lst =
  match lst with
    |[] -> false
    |h::t -> if h = n then true else determine n t;;

let rec app : 'a list -> 'a list -> 'a list
= fun l1 l2 -> 
  match l1 with
    |[] -> l2
    |h::t -> if determine h l2 then app t l2 else app t (l2 @ [h]);;

app [4;5;6;7] [1;2;3;4];;