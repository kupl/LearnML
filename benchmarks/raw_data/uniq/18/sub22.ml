let rec chck lst item =
  match lst with 
    | [] -> true
    | h::t -> if(h=item) then false else chck t item;;

let rec uniq : 'a list -> 'a list
= fun lst -> 
  match lst with 
    | [] -> []
    | h::t -> if(chck t h) then h::(uniq t) else (uniq t);;
    
uniq [5;6;5;4;1;54;2;5;2;2;5;2;3;4;2;1];;