let rec merge: int list * int list -> int list
= function
    (l1,[]) -> l1
    | ([],l2) -> l2
    | (a::l1,b::l2) -> if a > b then a::(merge (l1,b::l2)) else b::(merge (a::l1,l2))

