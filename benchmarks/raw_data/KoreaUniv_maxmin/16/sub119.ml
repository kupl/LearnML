(*Problem 1*)
let max_f:int->int->int
=fun a b->if a>b then a else b

let rec max:int list->int
=fun lst->match lst with
|[]->(-99999)
|hd::tl-> max_f hd (max tl) 
 