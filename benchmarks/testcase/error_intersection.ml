let rec f (l1: int list) (l2: int list) : int list =
  match l1 with
    | h1::t1 ->
    ( 
    	match l2 with
    	| h2::t2 -> if h1=h2 then h1::(f t1 l2) else (f t1 l2)
    	| [] -> []
    )
    | [] -> [] ;;