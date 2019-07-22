type ae = CONST of int
	| VAR of string
	| POWER of string * int
	| TIMES of ae list
	| SUM of ae list
exception InvalidArgument
let rec diff (ae, str) =
	match ae with
	| CONST(num) -> CONST 0
	| VAR(str2)-> if str=str2 then CONST 1 else CONST 0
	| POWER(str2,num) -> if str=str2 then 
				if num=1 then CONST 1
				else TIMES([CONST(num);POWER(str,num-1)])
				else CONST 0
	| SUM(aelist) -> if List.length(aelist)=0 then raise InvalidArgument
				else if List.length(aelist)=1 then diff(List.hd aelist, str)
				else SUM([diff((List.hd aelist), str);diff(SUM(List.tl aelist), str)])
	| TIMES(aelist) -> if List.length(aelist)=0 then raise InvalidArgument
                                else 
let rec timesdiff(aelist,indexnum,nownum)=
        if List.length(aelist)=1 then if indexnum=nownum
                                then diff(List.hd aelist, str)
                                else List.hd aelist
        else if indexnum=1 && List.length aelist > nownum then if indexnum=nownum
                                then SUM([TIMES([diff(List.hd aelist, str);
                                                timesdiff(List.tl aelist,2,nownum)]);
                                        timesdiff(aelist,1,nownum+1)])
                                else SUM([TIMES([List.hd aelist;
                                                timesdiff(List.tl aelist,2,nownum)]);
                                        timesdiff(aelist,1,nownum+1)])

        else if indexnum=nownum
        then TIMES([diff(List.hd aelist, str);
                timesdiff(List.tl aelist,indexnum+1,nownum)])
        else TIMES([List.hd aelist;
                timesdiff(List.tl aelist,indexnum+1,nownum)]) in
	timesdiff(aelist,1,1)
