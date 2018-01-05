exception Error of string

let numvocalize num =
	match num with
	0 -> "영"
	| 1 -> "일"
	| 2 -> "이"
	| 3 -> "삼"
	| 4 -> "사"
	| 5 -> "오"
	| 6 -> "육"
	| 7 -> "칠"
	| 8 -> "팔"
	| 9 -> "구"
	| _ -> raise (Error "numvocalize : input is not digit")

let rec subvocalize num =
	if (num/1000) = 1 then "천" :: (subvocalize (num mod 1000))
	else if (num/1000) != 0 then (numvocalize (num/1000)) :: "천" :: (subvocalize (num mod 1000))
	else if (num/100) = 1 then "백" :: (subvocalize (num mod 100))
	else if (num/100) != 0 then (numvocalize (num/100)) :: "백" :: (subvocalize (num mod 100))
	else if (num/10) = 1 then "십" :: (subvocalize (num mod 10))
	else if (num/10) != 0 then (numvocalize (num/10)) :: "십" :: (subvocalize (num mod 10))
	else if num = 0 then []
	else (numvocalize num) :: []

let vocalize s =
	let len = String.length s in
	if len = 7 then
		let fst = (int_of_string (String.sub s 0 3)) in
		let snd = (int_of_string (String.sub s 3 4)) in
		match (fst, snd) with
		(0,0) -> [["영"];["영"]]
		| (0,s) -> [["영"];subvocalize s]
		| (f,0) -> [subvocalize f;["영"]]
		| (f,s) -> [subvocalize f;subvocalize s]
	else if len = 8 then
		let fst = (int_of_string (String.sub s 0 4)) in
		let snd = (int_of_string (String.sub s 4 4)) in
		match (fst, snd) with
		(0,0) -> [["영"];["영"]]
		| (0,s) -> [["영"];subvocalize s]
		| (f,0) -> [subvocalize f;["영"]]
		| (f,s) -> [subvocalize f;subvocalize s]
	else raise (Error "vocalize : input length is not 7 or 8")
(*
let printlst lst =
	List.iter print_endline lst

let printans lst =
	print_string "[";
	printlst (List.hd lst);
	print_string ",";
	printlst (List.hd (List.tl lst));
	print_string "]";

*)
