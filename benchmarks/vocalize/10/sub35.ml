exception Error of string
let vocalize number =
let trans x =
	match x with 
	"1"->"일"
	|"2"->"이"
	|"3"->"삼"
	|"4"->"사"
	|"5"->"오"
	|"6"->"육"
	|"7"->"칠"
	|"8"->"팔"
	|"9"->"구"
	|_->raise (Error "not number")
	in
	let rec tokenize str = 
		if String.length str = 1 then [str]
		else (String.sub str 0 1)::tokenize (String.sub str 1 (String.length str -1)) in
	let rec translate str =
		match str with
		a::b->if a = "0" then (translate b)
			  else if (List.length b) = 0 then [(trans a)]
			  else if a="1" then(
			  if (List.length b) = 3 then "천"::(translate b)
			  else if (List.length b) = 2 then "백"::(translate b)
			  else if (List.length b) = 1 then "십"::(translate b)
		      else raise (Error "WTH")
				  )
			  
			  else if (List.length b) = 3 then (trans a)::"천"::(translate b)
			  else if (List.length b) = 2 then (trans a)::"백"::(translate b)
			  else if (List.length b) = 1 then (trans a)::"십"::(translate b)
		      else raise (Error "WTH")
				  
		|_->[]
		in
	let parsenumber str =
		[translate (tokenize (String.sub str 0 (String.length str -4))); translate (tokenize (String.sub str (String.length str -4) 4))] in
	
	if (String.length number)>8 || (String.length number)<7 then raise (Error "Sorry. You called to a wrong number.")
	else parsenumber number

