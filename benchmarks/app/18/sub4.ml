let rec chkList : 'a list -> int -> bool
= fun l num ->
  match l with
    | [] -> false
    | [a] -> if a = num then true else false
    | hd::tl -> if hd=num || (chkList tl num) then true else false;;

let rec app : 'a list -> 'a list -> 'a list
= fun l1 l2 ->
  match l1 with
    | [] -> l2
    | hd::tl -> if (chkList l2 hd) then (app tl l2) else app tl (l2 @ [hd]);;
    
app [4;5;6;7] [1;2;3;4];;
app [2;5;4;8] [8;2;5;4];;