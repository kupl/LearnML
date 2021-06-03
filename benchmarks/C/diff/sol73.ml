type aexp = Const of int
	| Var of string
	| Power of string * int
	| Times of aexp list
	| Sum of aexp list
exception InvalidArgument
let rec diff (aexp, str) =
	match aexp with
	| Const(num) -> Const 0
	| Var(str2)-> if str=str2 then Const 1 else Const 0
	| Power(str2,num) -> if str=str2 then 
				if num=1 then Const 1
				else Times([Const(num);Power(str,num-1)])
				else Const 0
	| Sum(aexplist) -> if List.length(aexplist)=0 then raise InvalidArgument
				else if List.length(aexplist)=1 then diff(List.hd aexplist, str)
				else Sum([diff((List.hd aexplist), str);diff(Sum(List.tl aexplist), str)])
	| Times(aexplist) -> if List.length(aexplist)=0 then raise InvalidArgument
                                else 
let rec timesdiff(aexplist,indexnum,nownum)=
        if List.length(aexplist)=1 then if indexnum=nownum
                                then diff(List.hd aexplist, str)
                                else List.hd aexplist
        else if indexnum=1 && List.length aexplist > nownum then if indexnum=nownum
                                then Sum([Times([diff(List.hd aexplist, str);
                                                timesdiff(List.tl aexplist,2,nownum)]);
                                        timesdiff(aexplist,1,nownum+1)])
                                else Sum([Times([List.hd aexplist;
                                                timesdiff(List.tl aexplist,2,nownum)]);
                                        timesdiff(aexplist,1,nownum+1)])

        else if indexnum=nownum
        then Times([diff(List.hd aexplist, str);
                timesdiff(List.tl aexplist,indexnum+1,nownum)])
        else Times([List.hd aexplist;
                timesdiff(List.tl aexplist,indexnum+1,nownum)]) in
	timesdiff(aexplist,1,1)
