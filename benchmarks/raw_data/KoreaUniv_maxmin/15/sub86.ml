let rec max l = 
	match l with [] -> 1(*empty list�� ������ 1�� ��ȯ*) 
	|hd::tl -> 
		if (tl=[]) then hd 
		else if (hd>max tl) then hd 
		else max tl 
 