
let rec checkRepeat l1 num = 
  match l1 with
    [] -> false
    |hd::tl -> if hd = num then true
               else checkRepeat tl num;;

let rec uniq_ l1 l2 = 
  match l2 with
    [] -> l1
    |hd::tl -> if ((checkRepeat l1 hd) = true) then uniq_ l1 tl
               else uniq_ (l1@[hd]) tl;;
               
let uniq l2 = uniq_ [] l2;;

(*test function*)
uniq [5; 6; 5; 4];;