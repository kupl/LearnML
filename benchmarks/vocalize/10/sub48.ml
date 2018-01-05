let hanglro n = (* 무조건 1이상 9 이하가 들어오도록 해줘! *)
	match n with
		1 -> "일"
		| 2 -> "이"
		| 3 -> "삼"
		| 4 -> "사"
		| 5 -> "오"
		| 6 -> "육"
		| 7 -> "칠"
		| 8 -> "팔"
		| 9 -> "구"
		| 0 -> "영"
		| _ -> "";;

let rec listconcat li1 li2 =
	match li1 with
		[] -> li2
		| x::y -> x::(listconcat y li2)


let hanglro_ n = 
	if n = 0 then ""
	else 
		if n = 1 then ""
		else hanglro n;;

let rec numberlister n =
	if n >= 10 then listconcat (numberlister (n/10)) [(n mod 10)]
	else [n];;

let rec listsize li =
	match li with
		[] -> 0
		| x::y -> 1 + (listsize y);;

let rec danwi jari =
	match jari with
		4 -> "천"
		| 3 -> "백"
		| 2 -> "십"
		| 5 -> String.concat "" ["만";(danwi (jari-4))]
		| 6 -> String.concat "" ["만";(danwi (jari-4))]
		| 7 -> String.concat "" ["만";(danwi (jari-4))]
		| 8 -> String.concat "" ["만";(danwi (jari-4))]
		| _ -> "";;

let onenumber head size =
	if size = 1 then [hanglro head]
	else 
		if head = 0 || head = 1 then [danwi size]
		else
			[hanglro head;danwi size]

let rec numlistgenerator numli =
	let lsize = (listsize numli) in
	if lsize = 0 
		then []
	else
		let head::tail = numli in
		listconcat (onenumber head lsize) (numlistgenerator tail);;

let rec cdr_last li n =
	let lsize = (listsize li) in
	if lsize <= n
		then li
	else
		let x::y = li in
		cdr_last y n;;

let rec car_last li n =
	let lsize = (listsize li) in
	if lsize <= n
		then []
	else
		let x::y = li in
		x::(car_last y n);;

exception Error of string

let vocalize str =
	let number = int_of_string str in
	let numli = numberlister number in
	let head = car_last numli 4 in
	let tail = cdr_last numli 4 in
	let headsize = listsize head in
	if headsize < 3 || headsize > 4 then raise (Error "Input Error")
	else [numlistgenerator head;numlistgenerator tail]

let rec numliprinter numli =
	match numli with
		[] -> ()
		| x::y -> (print_string x;numliprinter y)

let rec numliliprinter numlili =
	match numlili with
		[] -> ()
		| x::y -> (numliprinter x;print_string "-";numliliprinter y);;



