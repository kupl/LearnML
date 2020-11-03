let rec chck lst item =
  match lst with 
    | [] -> true
    | h::t -> if(h=item) then false else chck t item;;

let rec app l1 l2 =
  match l2 with
    | [] -> l1
    | h::t -> if(chck l1 h) then h::(app l1 t) else app l1 t;;
      
app [4;5;6;7] [1;2;3;4];;
app [2;5;3;6;2;1;3] [5;0;3;6;2;3;9];;
    