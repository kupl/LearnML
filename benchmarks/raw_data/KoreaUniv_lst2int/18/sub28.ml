let rec lst2int : int list -> int
= fun lst -> (*TODO*)
match List.rev lst with
  | [] -> 0
  | h::t -> h+10*lst2int (List.rev t);;
  
  
  lst2int [2;3;4;5];;
  