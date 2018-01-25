
(* problem 6*) let rec drop: 'a list ->int -> 'a list= fun l n->
if n >0 then 
match l with
|[] -> []
|hd::tl -> drop tl (n-1)
else l
