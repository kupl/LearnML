let rec max l = 
	match l with [] -> 1(*empty list를 받으면 1을 반환*) 
	|hd::tl -> 
		if (tl=[]) then hd 
		else if (hd>max tl) then hd 
		else max tl 
 