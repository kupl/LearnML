let rec app : 'a list -> 'a list -> 'a list
= fun l1 l2 -> match l2 with
  |[]->l1
  |hd::tl-> let rec remove a l =match l with
    |[]->[]
    |h::t->if h=a then t else h::(remove a t)
    in let uniq l1=match l1 with 
      |[]->[]
      |h::t->hd::remove hd (app l1 tl)
      in uniq l1;;
    
    
app [4;5;6;7] [1;2;3;4];;
