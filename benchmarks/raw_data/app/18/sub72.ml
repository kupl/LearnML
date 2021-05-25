let rec checker n lst =
  match lst with
    | [] -> [n]
    | hd::tl -> if (n=hd) then [] else checker n tl;;
      
let rec app : 'a list -> 'a list -> 'a list
= fun l1 l2 -> match l1 with
  | [] -> l2
  | hd::tl -> app tl (l2 @ (checker hd l2)) ;;
