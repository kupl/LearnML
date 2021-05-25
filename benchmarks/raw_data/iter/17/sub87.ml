(*자유전공학부 2013-13444 박하영*)

let rec iter ((n:int), (f:'a->'a)) (x:'a) : 'a  =
	 if n==0 then x
	 else iter(n-1,f)(f x)

