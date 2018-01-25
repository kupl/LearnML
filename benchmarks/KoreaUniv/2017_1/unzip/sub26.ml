(* problem 7*)

let rec makeList lst extract = 
match lst with |[]->[]|hd::tl->(extract hd)::makeList tl extract;;
let unzip : ('a * 'b) list -> 'a list * 'b list
= fun lst -> (makeList lst (fun tuple->match tuple with |(x,_)->x),
makeList lst (fun tuple->match tuple with |(_,y)->y));;