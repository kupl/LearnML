let rec zipper : int list * int list -> int list =fun (a,b) ->
match a with 
|[]->b
|hd::tl -> addlist hd (zipper (tl,b));

let rec addlist x l =
match l with 
|[]->[x]
|hd::tl -> if x>hd then hd::addlist x tl
else x::hd::tl;;
