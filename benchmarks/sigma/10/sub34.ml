(*hw1-1 ��ǻ�� ���к� 2008-11641 �����*) 

exception Error of string
let rec sigma (a, b, f) =
	if (a > b) || (a < 0) || (b < 0)  then
		raise (Error "Invalid input")
	else if (a = b) then 
		(f b)
	else 
		(f a) + (sigma ((a+1), b, f))

