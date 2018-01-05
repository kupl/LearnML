let rec merge ( a :int list , b :int list ) = 
	if a = [] then b
	else if b = [] then a
	else (
		if (List.hd a) >= (List.hd b) then List.hd a :: merge ((List.tl a), b)
		else List.hd b :: merge (a, (List.tl b))
		
	)


let x : int list = [8 ; 3 ; 1]
let y : int list = [8 ; 7; 4; 2]

let z : int list = merge x y

let a = List.hd z
let b = List.hd (List.tl z)
let c = List.hd (List.tl (List.tl z))

let _ = print_endline (string_of_int a)
let _ = print_endline (string_of_int b)
let _ = print_endline (string_of_int c)
let _ = z