let rec max l = 
	match l with [] -> 1(*empty list�� ������ 1�� ��ȯ*) 
	|hd::tl -> 
		if (tl=[]) then hd 
		else if (hd>max tl) then hd 
		else max tl 
let rec min l = 
	match l with [] -> 1(*empty list�� ������ 1�� ��ȯ*) 
	|hd::tl -> 
		if (tl=[]) then hd 
		else if (hd<min tl) then hd 
		else min tl 
