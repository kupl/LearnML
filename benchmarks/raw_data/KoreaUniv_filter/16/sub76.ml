let rec filter pred lst = 
	match lst with
	[] -> []
|	n::lst2 -> if pred n then n :: filter pred lst2 else filter pred lst2
