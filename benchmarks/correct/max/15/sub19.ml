let rec max l = 
   match l with 
       [x]->x
      | hd::tl-> if hd > (max tl) then hd else (max tl);;
 