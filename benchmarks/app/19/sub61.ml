(* 'a list -> 'a list -> 'a list*)

 let rec app l1 l2 =
 let rec match_list n l1 =
  match l1 with
    | [] -> [n]
    | hd::tl -> if n <> hd then hd::(match_list n tl) else match_list n tl in
  match l1 with
    | [] -> l2
    | hd::tl -> app tl (match_list hd l2) ;;
  




