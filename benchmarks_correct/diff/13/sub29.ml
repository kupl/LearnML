type aexp = Const of int
| Var of string
| Power of string * int
| Times of aexp list
| Sum of aexp list

exception InvalidArgument
let rec diff (a,str)=
	match a with
	|Const i-> Const 0
	|Var s->
		if s=str then Const 1
		else Const 0

	|Times aexpl->
		(match aexpl with
		|[]->raise InvalidArgument
		|hd::[]->diff(hd,str)
		|hd::tl->Sum[Times(diff(hd,str)::tl);Times[hd;diff(Times tl,str)]]
)
	|Sum aexpl->
		(match aexpl with
  		|[]->raise InvalidArgument
   		|hd::[]->diff(hd,str)
    	|hd::tl->Sum[diff(hd,str);diff(Sum tl,str)]
)
	|Power (s,i)-> Times[Const i;Power (s,i-1);diff(Var s,str)]
