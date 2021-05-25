let rec _contains lst value = 
  match lst with
    [] -> false
    | hd::tl -> 
      if hd = value then true
      else _contains tl value;;

let rec app : 'a list -> 'a list -> 'a list
= fun l1 l2 -> 
  match l1 with
    [] -> l2
    | hd::tl -> 
      if (_contains l2 hd) then (app tl l2)
      else app tl (l2@[hd]);;
