
let rec checkRepeat l1 num = 
  match l1 with
    [] -> false
    |hd::tl -> if hd = num then true
               else checkRepeat tl num;;

let rec app l1 l2 = 
  match l1 with
    [] -> l2
    |hd::tl -> if((checkRepeat l2 hd) = true) then app tl l2
               else app tl (l2@[hd]);;
               
(*test function*)
app [4;5;6;7] [1;2;3;4];;
app [4;5;1;7] [1;2;3;4];;