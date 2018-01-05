type teach_radix = (int* string) list ;;
type teach_count = (int* string) list;;
let tra= [(0,"") ; (1,"십") ; (2,"백"); (3,"천")];;
let tco =  [(0,"") ; (1,"일") ; (2,"이") ; (3,"삼") ; (4,"사") ; (5,"오"); (6,"육") ;(7,"칠");(8,"팔");(9,"구")];;
let etra= [(0,"") ; (1,"ten") ; (2,"hundred"); (3,"thousand")];;
let etco =  [(0,"") ; (1,"one") ; (2,"two") ; (3,"three") ; (4,"four") ; (5,"five"); (6,"six") ;(7,"seven");(8,"eight");(9,"nine")];;

exception Error of string;;

(* Problem 1.*)
let rec sigma_temp: int * int * (int -> int) -> int =
	(function (a,b,f) ->
		if (a = b) then (f a)
			else if (a < b) then ((f a) + (sigma_temp ((a+1), b,f)))
				else raise (Error "Start point is bigger than end point"));;
let sigma (a,b,f) =
	try
		sigma_temp (a,b,f)
	with Error t -> raise (Error t) 
		| e -> raise (Error "Invalid value.");;
(* Problem 2.*) 

let rec prodpi : (int * int -> float) * int * int -> float =
	(function (matr,i,k) ->
		if (k = 1) then (matr (i,1))
			else if (k < 1) then raise (Error "end point is invalid.")
				else ((matr (i,k)) *. (prodpi (matr,i,(k-1)))));;
let rec sumprod_temp : (int * int -> float) * int * int -> float =
	(function (matr,n,k) -> 
		if (n = 1) then (prodpi (matr,1,k))
			else if (n < 1) then raise (Error "end point is invalid.")
				else ((prodpi (matr,n,k)) +. (sumprod_temp (matr,(n-1),k))));;
let sumprod (matr,n,k) =
	try 
		sumprod_temp(matr,n,k)
	with Error t -> raise (Error t)
	| e -> raise (Error "Invalid value.");;
(* Problem 3.*)
let rec pow_int : int -> int -> int =
	(function a -> function n ->
		(if (n<0) then raise (Error "Invalid value.")
			else if (n= 0) then 1 
				else (a * (pow_int a (n-1)))));;
let rec converter : int -> (int * string) list -> string =
	(function ind -> function myls ->
		(match myls with
			[] -> raise (Error "Invalid value.")
			| (h::t) -> if ((fst h) = ind) then (snd h) else (converter ind t)));;
let rec mysize : int -> int =
	(function a ->
		(if (a > 9) then ((mysize (a/10)) + 1)
			else if ((a >= 0) && (a <= 9)) then 0
				else raise (Error "Invalid value.")))
let get_the_index_of : int -> int -> int = 
	(function number -> function myind -> 
		let inja = (pow_int 10 myind) in
			((number/inja) mod 10));;

let printer : string list -> unit =
	(function a -> (List.iter print_string a))
let rec inttovoice : int -> (teach_radix * teach_count) -> string list =
	(function number -> (function (ra,co) ->
		if (number < 0) then raise (Error "It is negative number.")
			else if (number > 9999) then raise (Error "The number is to high.")
				else if (number = 0) then []
					else let thesz = (mysize number) in
						let theid = (get_the_index_of number thesz) in
							if (theid = 0) then (inttovoice (number mod (pow_int 10 thesz)) (ra,co))
								else if ((theid = 1) && (thesz > 0)) then [(converter thesz ra)] @ (inttovoice (number mod (pow_int 10 thesz)) (ra,co))
								else [(converter theid co)] @ [(converter thesz ra)] @ (inttovoice (number mod (pow_int 10 thesz)) (ra,co))));;
let makevoice : string -> (teach_radix * teach_count) -> string list =
	(function thenum -> (function (ra,co) ->
		let number = (int_of_string thenum) in
			if (number < 0) then raise (Error "Invalid error.")
				else if (number = 0) then ["영"]
					else (inttovoice number (ra,co))))
let vocalize_temp : string -> string list list =
	(function thenum ->
		let thelen = (String.length thenum) in
			if (thelen = 7) then [(makevoice (String.sub thenum 0 3) (tra,tco))] @ [(makevoice (String.sub thenum 3 4) (tra,tco))]
				else if (thelen = 8) then [(makevoice (String.sub thenum 0 4) (tra,tco))] @ [(makevoice (String.sub thenum 4 4) (tra,tco))]
					else raise (Error "Is error."))

let vocalize : string -> string list list =
	(function thenum ->
		try
			(vocalize_temp thenum)
		with e -> raise (Error "Invalid value."))
