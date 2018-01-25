(*problem7*)
let p7_subfunc : 'a->'b->'a list*'b list->'a list*'b list = fun x y (lst1,lst2)-> (lst1@[x],lst2@[y])
let rec unzip2 : ('a*'b) list -> 'a list*'b list -> 'a list * 'b list = fun lst (lst1,lst2) ->
	match lst with 
		hd::tl->let (x,y) = hd in
						let tmp = p7_subfunc x y (lst1,lst2) in
						unzip2 tl tmp
    |[]->(lst1,lst2)
let unzip : ('a*'b)list -> 'a list * 'b list = fun lst -> unzip2 lst([],[]);;