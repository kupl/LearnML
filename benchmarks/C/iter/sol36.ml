(*2009-11718 1-2*)
let rec iter (n, f) a =
	if n<0 then raise(Invalid_argument "error")
	else if n=0 then a
	else (f (iter (n-1, f) a))

(* ��ǲ ���°� �³� ���� f �Լ��� �� ���� *) 
