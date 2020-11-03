let rec drop
=fun lst n->
  match lst with
    |[] -> []
    |hd::tl -> if hd=n then (drop tl n) else hd::(drop tl n);;
    
let rec uniq : 'a list -> 'a list
= fun lst ->
  match lst with
    |[] -> []
    |hd::tl -> hd::(uniq (drop tl hd));;
    
let rec app : 'a list -> 'a list -> 'a list
= fun l1 l2 -> uniq(l2@l1);;

app [1;1;1;2][3;3;2];;