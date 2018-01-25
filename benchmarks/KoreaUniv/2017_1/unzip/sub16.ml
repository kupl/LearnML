(* problem 7*)
let unzip : ('a * 'b) list -> 'a list * 'b list
= fun lst ->
  let rec iterate = fun l al bl ->
    match l with
    | [] -> (al,bl)
    | hd::tl -> iterate tl (al@[fst hd]) (bl@[snd hd])
  in
  iterate lst [] []
;;