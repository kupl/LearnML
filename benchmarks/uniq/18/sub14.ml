let rec checkDuplicate : 'a -> 'a list -> bool
= fun a lst ->
  match lst with
  | [] -> false
  | hd::tl -> if (a = hd) then true else (checkDuplicate a tl)
;;

let rec uniq : 'a list -> 'a list
= fun lst ->
  match lst with
  | [] -> []
  | hd::tl -> let temp = hd in
                match (checkDuplicate temp tl) with
                | true -> uniq tl
                | false -> lst
;;


(*
[checkDuplicate : 'a -> 'a list -> bool] checks whether a given element is in a given list.
*)