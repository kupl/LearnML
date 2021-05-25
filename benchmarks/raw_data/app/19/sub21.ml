let rec sub_rmv_dup l1 l2_elm =
  match l1 with
    | [] -> []
    | hd::tl -> if ( hd = l2_elm ) then ( sub_rmv_dup tl l2_elm ) else hd::( sub_rmv_dup tl l2_elm );;

let rec rmv_dup l1 l2 = 
  match l2 with
    | [] -> l1
    | hd::tl -> rmv_dup ( sub_rmv_dup l1 hd ) tl;;
  
let rec sub_app l1 l2 =
  match l1 with
    | [] -> l2
    | hd::tl -> sub_app tl ( l2 @ [hd] );;

let app : 'a list -> 'a list -> 'a list
= fun l1 l2 -> (* TODO *)
  sub_app (rmv_dup l1 l2) l2;;

