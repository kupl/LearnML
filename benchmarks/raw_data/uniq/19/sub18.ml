let rec isuniq = fun h lst ->
  match lst with
    | [] -> true
    | hd::tl -> if h = hd then false else isuniq h tl;;

let uniq : 'a list -> 'a list
= fun lst -> (* TODO *)
let rec f = fun l1 l2 ->
  match l1 with
    | [] -> l2
    | hd::tl -> if (isuniq hd l2) then (f tl (l2@[hd])) else (f tl l2)
  in f lst [];;



