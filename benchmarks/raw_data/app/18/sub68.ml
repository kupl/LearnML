let rec mycheck : 'a list -> 'a -> bool
= fun lst e ->
    match lst with
      | [] -> true
      | hd::tl ->
          if (hd = e) then false
          else (mycheck tl e);;

let rec app : 'a list -> 'a list -> 'a list
= fun l1 l2 ->
    match l1 with
      | [] -> l2
      | hd::tl ->
          if (mycheck l2 hd) then (app tl (l2@[hd]))
          else (app tl l2);;

(*
app [4;5;6;7] [1;2;3;4];;
*)
