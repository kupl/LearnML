(*p6*)

let rec drop : 'a list->int->'a list
=fun l n -> if n=0 then l
else drop (match l with |[]->[]| h::t->t) (n-1);;