module L=List
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
	| SUM(aelist) -> if L.length(aelist)=0 then raise InvalidArgument
				else if L.length(aelist)=1 then diff(L.hd aelist, str)
				else SUM([diff((L.hd aelist), str);diff(SUM(L.tl aelist), str)])
	| TIMES(aelist) -> if L.length(aelist)=0 then raise InvalidArgument
                                else 
let rec timesdiff(aelist,indexnum,nownum)=
        if L.length(aelist)=1 then if indexnum=nownum
                                then diff(L.hd aelist, str)
                                else L.hd aelist
        else if indexnum=1 && L.length aelist > nownum then if indexnum=nownum
                                then SUM([TIMES([diff(L.hd aelist, str);
                                                timesdiff(L.tl aelist,2,nownum)]);
                                        timesdiff(aelist,1,nownum+1)])
                                else SUM([TIMES([L.hd aelist;
                                                timesdiff(L.tl aelist,2,nownum)]);
                                        timesdiff(aelist,1,nownum+1)])

        else if indexnum=nownum
        then TIMES([diff(L.hd aelist, str);
                timesdiff(L.tl aelist,indexnum+1,nownum)])
        else TIMES([L.hd aelist;
                timesdiff(L.tl aelist,indexnum+1,nownum)]) in
	timesdiff(aelist,1,1)
