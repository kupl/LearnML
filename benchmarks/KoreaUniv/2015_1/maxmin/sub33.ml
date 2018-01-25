let rec max l = 
	match l with
	| [] -> 
	| [a] -> a
	| hd::tl -> if hd < max(tl) then max(tl) else hd;;

let rec min l = 
	match l with
	| [] -> 
	| [a] -> a
	| hd::tl -> if hd > min(tl) then min(tl) else hd;;
