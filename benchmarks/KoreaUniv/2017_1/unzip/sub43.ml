(* problem 7*)

let rec unzip_x l = 
	match l with
	| [] -> []
	| hd::tl -> match hd with (x,_) -> [x]@unzip_x tl

let rec unzip_y l = 
	match l with
	| [] -> []
	| hd::tl -> match hd with (_,y) -> [y]@unzip_y tl

let unzip : ('a * 'b) list -> 'a list * 'b list
= fun lst -> ((unzip_x lst), (unzip_y lst))