let rec addToListMap f l =
	match l with
	| [] -> []
	| hd::tl -> if(f hd) then hd::addToListMap f tl
							else addToListMap f tl 

let rec filter pred lst = addToListMap pred lst
