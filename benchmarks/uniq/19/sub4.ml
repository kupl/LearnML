let rec isin n lst = 
  match lst with
    | [] -> false
    | hd::tl -> if n = hd then true else isin n tl ;;

let rec union l1 l2 = 
  match l1 with
    | [] -> l2
    | hd::tl -> if isin hd l2 then union tl l2 else union tl (l2@[hd]) ;;

let uniq : 'a list -> 'a list
= fun lst -> union lst [] ;;

uniq [5;6;5;4] ;;
uniq [1;1;1;1;2;1;1;1;8;1;2;1;1;5;1;1] ;;