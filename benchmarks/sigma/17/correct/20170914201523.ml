(*자유전공학부 2013-13444 박하영*)

let rec sigma ((a:int), (b:int), (f:int->int)) : int =
	 if b < a then 0
	 else f(a)+sigma(a+1, b, f)
