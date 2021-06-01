let rec mycheck : 'a -> 'a list -> bool
= fun e lst ->
    match lst with
      | [] -> true
      | hd::tl ->
          if (hd = e) then false
          else (mycheck e tl);;

let rec myuniq : 'a list -> 'a list -> 'a list
= fun lst newlst ->
    match lst with
      | [] -> newlst
      | hd::tl ->
          if (mycheck hd newlst) = true then (myuniq tl (newlst@[hd]))
          else (myuniq tl newlst);;

let rec uniq : 'a list -> 'a list
= fun lst -> myuniq lst [];;

(*
uniq [5;6;5;4;];;
*)
