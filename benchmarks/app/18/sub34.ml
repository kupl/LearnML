let rec isin : 'a -> 'a list -> bool
= fun a lst ->
  match lst with
    |[] -> false
    |h::t -> if h = a then true else isin a t;;

let rec app : 'a list -> 'a list -> 'a list
= fun l1 l2 ->
  match l1 with
    |[] -> l2
    |h::t -> if isin h l2 then app t l2 else app t (l2@[h]);;
