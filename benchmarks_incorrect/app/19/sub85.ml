let rec check_duplicate = fun a lst ->
  match lst with
    | [] -> false
    | hd::tl -> if a = hd then true else check_duplicate a tl
;;

let rec remove_duplicate = fun lst ->
  match lst with
    | [] -> []
    | h1::t1 -> (match t1 with
                  | [] -> [h1]
                  | h2::t2 -> if h1 = h2 then (remove_duplicate(h1::t2)) else h1::(remove_duplicate(t1))
                );;

let rec app : 'a list -> 'a list -> 'a list
= fun l1 l2 ->
  let l2 = remove_duplicate l2 in 
  (match l1 with
    | [] -> l2
    | hd::tl -> if check_duplicate hd l2 then app tl l2 else app tl (l2@[hd]) 
  );;
