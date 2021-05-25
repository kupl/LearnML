let rec thereis a l =
  match l with
    [] -> false
    |hd::tl -> if (a=hd) then true else (thereis a tl);;

let rec uniq : 'a list -> 'a list
= fun lst -> 
  match lst with
    [] -> []
    |hd::tl -> if (thereis hd tl) then uniq tl else hd::(uniq tl);;
    
uniq [5;6;5;4];;
