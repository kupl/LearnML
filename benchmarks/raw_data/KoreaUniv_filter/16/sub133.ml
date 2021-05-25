let rec filter pred lst = 
	match lst with
	| [] -> []
	| hd::tl -> (if pred hd then [hd] else []) @ filter pred tl;;
