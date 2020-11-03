let rec comp h lst =
  match lst with
    [] -> false
    |hd::tl -> if (h == hd) then true
              else comp h tl;;

let rec app : 'a list -> 'a list -> 'a list
= fun l1 l2 ->
  match l1 with
    [] -> l2
    |hd::tl -> if comp hd l2  then app tl l2
              else app tl (l2 @ [hd]);;