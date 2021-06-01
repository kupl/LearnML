(* HW 1-2 / 2007-11603 / ��ǻ�Ͱ��к� / �̿��� *)

let rec iter (n, f) x =
	let identity n = n in

	if n < 0 then
		raise (Invalid_argument "iter")
	else if n = 0 then
		(identity x)
	else
		(f (iter (n-1, f) x))
		