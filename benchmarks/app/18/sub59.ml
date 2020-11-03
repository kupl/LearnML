let rec isUniq(* True if m is unique in lst  *)
= fun lst m -> match lst with 
  | [] -> true
  | hd::tl -> if hd=m then false else (isUniq tl  m);;




let rec app : 'a list -> 'a list -> 'a list
= fun l1 l2 -> match l2 with
  | [] -> l1
  | hd::tl ->if (isUniq l1 hd) then app (l1@[hd]) tl else app l1 tl;;
  
  
app [1;2;3] [3;4;5];;

app [1;2;3] [4;5;6];;

app [1;2;1] [3;2;4];;