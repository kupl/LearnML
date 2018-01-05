let rec merge((x: int list),(y :int list)) =
	match x,y with
	| ([],l) -> l
	| (l,[]) -> l
	| (a::x' ,b::y') -> if a > b then a :: merge(x',y) else b :: merge(x,y') 