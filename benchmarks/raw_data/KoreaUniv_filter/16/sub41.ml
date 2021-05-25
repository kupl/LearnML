let rec filter pred lst = match lst with
  	| [] -> []
  	| [a] -> if pred a then [a] else []
  	| hd::tl -> if pred hd then hd::(filter pred tl) else filter pred tl
