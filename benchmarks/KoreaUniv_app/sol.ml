let rec is_mem : 'a -> 'a list -> bool
= fun e lst ->
  match lst with
  | [] -> false
  | hd::tl -> (hd = e) || (is_mem e tl)

let rec app_uniq : 'a list -> 'a list -> 'a list
= fun l1 l2 ->
  match l1 with
  | [] -> l2
  | hd::tl -> if (is_mem hd l2) then app_uniq tl l2 else app_uniq tl (l2@[hd])
    
let rec app : 'a list -> 'a list -> 'a list
= fun l1 l2 -> app_uniq l1 (app_uniq l2 [])
;;