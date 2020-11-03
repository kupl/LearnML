let rec chack
=fun n lst -> match lst with
  |[] -> false
  |hd::tl -> (hd=n)||(chack n tl);;


let rec app : 'a list -> 'a list -> 'a list
= fun l1 l2 -> 
   match l2 with
     |[] -> l1
     |hd::tl -> if chack hd l1 then app l1 tl else app(hd::l1)tl;;
     
   
   
     app[4;5;6;7][1;2;3;4];;
     
     
