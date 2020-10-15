(*자유전공학부 2013-13444 박하영*)

let rec sigma f a b =
	 if b < a then 0
	 else f(a)+sigma f (a+1) b
