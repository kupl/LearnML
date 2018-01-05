(* Exercise 3 *)
exception Error of string

(* Function 'print_strlist' is for debug *)
let rec print_strlist strlist =
	print_string ((String.concat "" strlist) ^ "\n");

exception INVALID_DIGIT

let numbertostr n =
	match n with
		'0' ->
			"영"
		| '1' ->
			"일"
		| '2' ->
			"이"
		| '3' ->
			"삼"
		| '4' ->
			"사"
		| '5' ->
			"오"
		| '6' ->
			"육"
		| '7' ->
			"칠"
		| '8' ->
			"팔"
		| '9' ->
			"구"
		| _ ->
			raise INVALID_DIGIT

exception INVALID_DIGITWEIGHT

let digittostr n i =
	match n with
		'0' ->
			[]
		| _ ->
			let number = [(numbertostr n)] in
			let weight =
				match i with
					0 ->
						[]
					| 1 ->
						["십"]
					| 2 ->
						["백"]
					| 3 ->
						["천"]
					| _ ->
						raise INVALID_DIGITWEIGHT
				in
				match (n,i) with
					('1', 0) ->
						["일"]
					| ('1', _) ->
						weight
					| _ ->
						(List.append number weight)

let rec padzeroto4 str =
	if ((String.length str) = 4) then
		str
	else if ((String.length str) < 4) then
		padzeroto4 ("0" ^ str)
	else
		"0000"
			

exception INVALID_LENGTH_NUMBER

let intstrtostrlist str =
	if ((String.length str) > 4) then
		raise INVALID_LENGTH_NUMBER
	else
		let str = (padzeroto4 str) in
		(match str with
			"0000" ->
				["영"]
			| _ ->
				(List.fold_right (List.append) [(digittostr str.[0] 3); (digittostr str.[1] 2); (digittostr str.[2] 1); (digittostr str.[3] 0)] [])
		)

let vocalize str =
	if (((String.length str) > 8) || ((String.length str) < 7)) then
		raise (Error "Length of string must be 7 or 8")
	else
		[(intstrtostrlist (String.sub str 0 ((String.length str) - 4))); (intstrtostrlist (String.sub str ((String.length str) - 4) 4))]

(* Function 'print_vocalized' is for debug *)
let print_vocalized str =
	let str = (vocalize str) in
	print_strlist (List.concat str)
