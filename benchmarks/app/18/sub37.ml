let rec thereis a l =
  match l with
    [] -> false
    |hd::tl -> if (a=hd) then true else (thereis a tl);;

let rec uniq : 'a list -> 'a list
= fun lst -> 
  match lst with
    [] -> []
    |hd::tl -> if (thereis hd tl) then uniq tl else hd::(uniq tl);;
    

let rec app : 'a list -> 'a list -> 'a list
= fun l1 l2 -> uniq (l1 @ l2);;

app [4;5;6;7][1;2;3;4];;

