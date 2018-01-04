type ae = CONST of int
| VAR of string
| POWER of string * int
| TIMES of ae list
| SUM of ae list

exception InvalidArgument
let rec diff (a,str)=
	match a with
	|CONST i-> CONST 0
	|VAR s->
		if s=str then CONST 1
		else CONST 0

	|TIMES ael->
		(match ael with
		|[]->raise InvalidArgument
		|hd::[]->diff(hd,str)
		|hd::tl->SUM[TIMES(diff(hd,str)::tl);TIMES[hd;diff(TIMES tl,str)]]
)
	|SUM ael->
		(match ael with
  		|[]->raise InvalidArgument
   		|hd::[]->diff(hd,str)
    	|hd::tl->SUM[diff(hd,str);diff(SUM tl,str)]
)
	|POWER (s,i)-> TIMES[CONST i;POWER (s,i-1);diff(VAR s,str)]
