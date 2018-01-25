let insert_list a l=
match l with
|[]->[a]
|hd::tl->l@[a];;

let fst p=match p with (x,_)->x;;
let scd p=match p with (_,x)->x;;

let rec mklst1: ('a*'b) list ->'a list
=fun lst->
match lst with
|[]->[]
|hd::tl->(insert_list (fst hd) [])@(mklst1 tl);;

let rec mklst2: ('a*'b) list->'b list
=fun lst->
match lst with
|[]->[]
|hd::tl->(insert_list (scd hd) [])@(mklst2 tl);;


let unzip:('a*'b) list ->'a list*'b list
=fun lst->((mklst1 lst),(mklst2 lst));;
