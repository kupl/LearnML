let app : 'a list -> 'a list -> 'a list
  = fun l1 l2 -> 
  let rec help l1 l2 ltemp = match l2 with
  | [] -> let rec checkdrop lt l = match lt with
          | [] -> []
          | hd::tl ->  let rec gtl e ls = match ls with 
                  | [] -> false
                  | h::t -> if e=h then true else gtl e t
                in if gtl hd l then checkdrop tl l else hd::checkdrop tl l
          in checkdrop l1 ltemp
  | hd::tl -> hd::(help l1 tl ltemp)
  in help l1 l2 l2;;
  
