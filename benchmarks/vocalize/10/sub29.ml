(*hw1-3 컴퓨터 공학부 2008-11641 신희식*) 

exception Error of string

let vocalize str =
	let trans_first a =
		match a with
		'1' -> ["일"]
		|'2' -> ["이"]
		|'3' -> ["삼"]
		|'4' -> ["사"]
		|'5' -> ["오"]
		|'6' -> ["육"]
		|'7' -> ["칠"]
		|'8' -> ["팔"]
		|'9' -> ["구"]
		|_ -> []
	in
	let trans a=
		match a with
		'1' -> []
		|'2' -> ["이"]
		|'3' -> ["삼"]
		|'4' -> ["사"]
		|'5' -> ["오"]
		|'6' -> ["육"]
		|'7' -> ["칠"]
		|'8' -> ["팔"]
		|'9' -> ["구"]
		|_ -> []
	in
	let transform t =
		match t with
		('0','0','0','0') -> 
			["영"]
		|('0','0','0',a) -> 
			(trans_first a)
		|('0','0',a,b) -> 
			(List.append (trans a) ("십"::(trans_first b)))
		|('0',a,'0',b) ->
			(List.append (trans a) ("백"::(trans_first b)))
		|('0',a,b,c) ->
			(List.append (trans a) 
			 	(List.append ("백"::(trans b)) ("십"::(trans_first c))))
		|(a,'0','0',b) ->
			(List.append (trans a) ("천"::(trans_first b)))
		|(a,'0',b,c) ->
			(List.append (trans a) 
				 	(List.append ("천"::(trans b)) ("십"::(trans_first c))))
		|(a,b,'0',c) ->
			(List.append (trans a) 
				 	(List.append ("천"::(trans b)) ("백"::(trans_first c))))
		|(a,b,c,d) ->
			(List.append (trans a) 
			 	(List.append ("천"::(trans b))
				 	(List.append ("백"::(trans c)) ("십"::(trans_first d)))))
	in
	match String.length str with
	7 ->
		(transform ('0',(String.get str 0),(String.get str 1),(String.get str 2)))
		::[(transform ((String.get str 3),(String.get str 4),(String.get str 5),(String.get str 6)))]
	|8 ->
		(transform ((String.get str 0),(String.get str 1),(String.get str 2),(String.get str 3)))
		::[(transform ((String.get str 4),(String.get str 5),(String.get str 6),(String.get str 7)))]
	|_ -> 
		raise (Error "Invalid input")
