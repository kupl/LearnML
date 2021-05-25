let rec isin a lst =
  match lst with
    [] -> false
  | hd::tl -> if a = hd then true else isin a tl;;

let rec app : 'a list -> 'a list -> 'a list
= fun l1 l2 -> 
  match l1 with
    [] -> l2
  | hd::tl -> if (isin hd l2) then (app tl l2) else (app tl (l2@[hd]));;

app [4;5;6;7] [1;2;3;4];;