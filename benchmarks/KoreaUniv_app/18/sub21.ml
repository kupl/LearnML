let rec check l1 l2 = 
  match l1, l2 with
    | l1, [] -> true
    | [], l2 -> false
    | hd1 :: tl1, hd2 :: tl2 -> if hd1 = hd2 then false else check l1 tl2 ;;

let rec app : 'a list -> 'a list -> 'a list
= fun l1 l2 -> 
   match l1 with
    | [] -> l2
    | hd :: tl -> 
      if check l1 l2 then app tl (l2 @ [hd])
      else app tl l2 ;;