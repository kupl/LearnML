(*2009-11718 1-2*)
let rec iter (n, f) a =
	if n<0 then raise(Invalid_argument "error")
	else if n=0 then a
	else (f (iter (n-1, f) a))

(* 인풋 형태가 맞나 저게 f 함수는 또 뭘로 *) 
