exception Error of string
let vocalize number =
let trans x =
	match x with 
	"1"->"1"
	|"2"->"2"
	|"3"->"3"
	|"4"->"4"
	|"5"->"5"
	|"6"->"6"
	|"7"->"7"
	|"8"->"8"
	|"9"->"9"
	|_->raise (Error "not number")
	in
	let rec tokenize str = 
		if String.length str = 1 then [str]
		else (String.sub str 0 1)::tokenize (String.sub str 1 (String.length str -1)) in
	let rec translate str =
		match str with
		a::b->if a = "0" then (translate b)
			  else if (List.length b) = 3 then (trans a)::"1000"::(translate b)
			  else if (List.length b) = 2 then (trans a)::"100"::(translate b)
			  else if (List.length b) = 1 then (trans a)::"10"::(translate b)
			  else if (List.length b) = 0 then [(trans a)]
		      else raise (Error "WTH")
		|_->[]
		in
	let parsenumber str =
		[translate (tokenize (String.sub str 0 (String.length str -4))); translate (tokenize (String.sub str (String.length str -4) 4))] in
	
	if (String.length number)>8 || (String.length number)<7 then raise (Error "Sorry. You called to a wrong number.")
	else parsenumber number

