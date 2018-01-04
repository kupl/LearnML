type ae = CONST of int
| VAR of string
| POWER of string * int
| TIMES of ae list
| SUM of ae list


let rec diff expr v =
	let rec f_times lst out= 
		match lst with
		|[] -> []
		|hd::tl -> (TIMES((diff hd v)::out@tl))::(f_times tl (hd::out))
	and
	f_sum lst = 
		match lst with
		|[] -> []
		|hd::tl -> (diff hd v)::(f_sum tl)
	in
	match expr with
	|CONST _ -> CONST 0
	|VAR v -> CONST 1
	|VAR _ -> CONST 0
	|POWER(v,n)-> if n = 0 then CONST 0 else if n = 1 then CONST 1 else TIMES [CONST n;POWER(v,(n-1))]
	|POWER _ -> CONST 0
	|TIMES lst -> SUM(f_times lst [])
	|SUM lst -> SUM(f_sum lst)
