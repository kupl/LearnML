let uniq : 'a list -> 'a list
= fun lst -> 
let rec reverse l a = match l with 
    | [] -> a
    | hd::tl -> reverse tl (hd::a) in
let rec checkdrop lt l = match lt with
  | [] -> l
  | hd::tl -> let rec gtl e ls = match ls with 
          | [] -> false
          | h::t -> if e=h then true else gtl e t
        in if gtl hd l then checkdrop tl l else checkdrop tl (hd::l)
in reverse (checkdrop lst []) [];;

