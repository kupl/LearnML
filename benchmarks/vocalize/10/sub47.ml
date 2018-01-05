exception Error of string

let emptyChecker voice =
	if voice = [] then ["ZERO"]
	else voice

let stringDivider str = 
	if (String.length str = 7) then 
		((String.sub str 0 3), (String.sub str 3 4))
	else if (String.length str = 8) then 
			((String.sub str 0 4), (String.sub str 4 4))
		 else
		 	raise (Error "Invalid Length")

let nextStr str =
	String.sub str 0 ((String.length str)-1)

let addDigit digit =
	match digit with
	 1 -> "½Ê"
	 | 2 -> "¹é"
	 | 3 -> "Ãµ"
	 | _ -> raise (Error "Digit")



let rec translator str digit voicelist =
	if (String.length str = 0) then voicelist
	else
		(match (str.[(String.length str)-1]) with
			  '0' -> translator (String.sub str 0 ((String.length str)-1)) (digit + 1) voicelist
			| '1' -> (if (digit = 0) then translator (String.sub str 0 ((String.length str)-1)) (digit + 1) ("ÀÏ"::voicelist)
					  else if (digit = 1) then  
					  	translator (String.sub str 0 ((String.length str)-1)) (digit + 1) ("½Ê"::voicelist)
					 	   else if (digit = 2) then 
						   	translator (String.sub str 0 ((String.length str)-1)) (digit + 1) ("¹é"::voicelist)
						  	    else if (digit = 3) then "Ãµ"::voicelist
							   	     else raise (Error "Digit"))
					 	
			| '2' -> ( if digit = 0 then translator (nextStr str) (digit + 1) ("ÀÌ"::voicelist)
					   else translator (nextStr str) (digit + 1) ("ÀÌ"::((addDigit digit)::voicelist)))
			| '3' ->( if digit = 0 then translator (nextStr str) (digit + 1) ("»ï"::voicelist)
					   else translator (nextStr str) (digit + 1) ("»ï"::((addDigit digit)::voicelist)))
			| '4' ->( if digit = 0 then translator (nextStr str) (digit + 1) ("»ç"::voicelist)
					   else translator (nextStr str) (digit + 1) ("»ç"::((addDigit digit)::voicelist)))
			| '5' ->( if digit = 0 then translator (nextStr str) (digit + 1) ("¿À"::voicelist)
					   else translator (nextStr str) (digit + 1) ("¿À"::((addDigit digit)::voicelist)))
			| '6' ->( if digit = 0 then translator (nextStr str) (digit + 1) ("À°"::voicelist)
					   else translator (nextStr str) (digit + 1) ("À°"::((addDigit digit)::voicelist)))
			| '7' ->( if digit = 0 then translator (nextStr str) (digit + 1) ("Ä¥"::voicelist)
					   else translator (nextStr str) (digit + 1) ("Ä¥"::((addDigit digit)::voicelist)))
			| '8' ->( if digit = 0 then translator (nextStr str) (digit + 1) ("ÆÈ"::voicelist)
					   else translator (nextStr str) (digit + 1) ("ÆÈ"::((addDigit digit)::voicelist)))
			| '9' ->( if digit = 0 then translator (nextStr str) (digit + 1) ("±¸"::voicelist)
					   else translator (nextStr str) (digit + 1) ("±¸"::((addDigit digit)::voicelist)))
			| _ -> raise (Error "Invalid input")
		)

let vocalize str =
	let vocalize_in (a,b) =
		(emptyChecker (translator a 0 []))::[(emptyChecker (translator b 0 []))]
	in
		vocalize_in (stringDivider str)
			
			

;;
