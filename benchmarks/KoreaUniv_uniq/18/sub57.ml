let rec in_it = fun l1 x -> (**)
  match l1 with
    |[] -> false
    |_-> if List.hd l1 = x then true else in_it (List.tl l1) x;;

let rec erase = fun lst x ->
  match lst with
    |[]->[]
    |hd::tl-> if hd = x then (erase tl x) else [hd] @ erase tl x;;

let rec uniq : 'a list -> 'a list
= fun lst -> 
  match lst with 
    |[]-> []
    |hd::tl-> if in_it tl hd = true then hd::uniq (erase tl hd) else hd::uniq tl;;
    
uniq [3;2;5;6;6;6;5;4;3;2];;
 