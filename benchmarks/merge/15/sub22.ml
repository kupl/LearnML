(*ex1*)
let rec merge ((a:int list), (b:int list)): int list  =
	match (a,b) with
	| (c,[]) -> c 
	| ([],d) -> d
	| (h1::t1,h2::t2) ->
		if h1>=h2 then h1::(merge (h2::t2, t1))
		else h2::(merge (h1::t1, t2))

  