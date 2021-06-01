let rec remove lst a =
  match lst with
    | [] -> []
    | hd::tl -> if a = hd then remove tl a else hd::(remove tl a);;

let rec uniq : 'a list -> 'a list
= fun lst -> 
  match lst with
   | [] -> []
   | hd::tl -> hd::(uniq (remove tl hd));; 
   
uniq [5;6;5;5;4;3;3;2;1;5;6];;(* TODO *)