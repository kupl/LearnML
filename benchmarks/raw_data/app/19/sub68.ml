let rec app : 'a list -> 'a list -> 'a list
= fun l1 l2 ->
  match l1 with
   [] -> l2
   |hd::tl -> let rec chk x lst =
     match lst with
       [] -> true
       |h::t -> if h = x then false else chk x t
      in if chk hd l2 then app tl (l2@[hd]) else app tl l2;;
      
app [4;5;6;7] [1;2;3;4];;
app [2;3;4] [1;3;5];;