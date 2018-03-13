let rec filter pred lst = 
match lst with
| [] -> []
| hd::lst' -> if pred hd 
	      then hd :: (filter pred lst') 
	      else filter pred lst'
