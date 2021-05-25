exception InvalidInput		(* Exception : �ݺ� Ƚ�� n�� ������ ��� *)

let rec iter (n, f) x =
	if n<0 then raise InvalidInput
	else if n=0 then x
	else f (iter (n-1, f) x)
