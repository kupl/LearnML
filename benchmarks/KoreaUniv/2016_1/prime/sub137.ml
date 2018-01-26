exception Error of string
let rec prime : int -> bool
= fun n ->
	if n<=0 then raise(Error "invalid arg")
	else let rec noDivisors (m :int ) : bool =
			m*m>n||(n mod m <>0 && noDivisors(m+1)) in n>=2&&noDivisors 2
