let rec app : 'a list -> 'a list -> 'a list
= fun al bl ->
  match al with
  | [] -> bl
  | h::t -> if (isIn bl h) then app t bl
    else app t (bl@[h])
and isIn l a =
  match l with
  | [] -> false
  | h::t -> if h = a then true else isIn t a;;


app [1;2;3;4] [1;1;3;3];;