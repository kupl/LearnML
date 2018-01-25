(*p7*)

let rec unzip : ('a*'b) list -> 'a list * 'b list
= fun lst-> match lst with
|[]->([],[])
|[('a,'b)]->(['a],['b])
|('a,'b)::t-> let (l1,l2) = unzip t in (['a],['b]);;