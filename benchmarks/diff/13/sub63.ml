type ae = CONST of int
| VAR of string
| POWER of string * int
| TIMES of ae list
| SUM of ae list

exception InvalidArgument  
  let rec diff_temp (ae, str) =
  match ae with
  CONST(n)->CONST(0)
  | VAR(str1)->if(str1=str) then CONST(1) else CONST(0)
  | POWER(str1,n)->if(str1=str) then TIMES([CONST(n); POWER(str1, n-1)]) else CONST(0)
  | TIMES(aelist)->if(List.tl aelist=[]) then diff_temp(List.hd aelist, str)
  else SUM(TIMES(diff_temp(List.hd aelist, str)::TIMES(List.tl aelist)::[])::TIMES(List.hd aelist::diff_temp(TIMES(List.tl aelist), str)::[])::[])
  | SUM(aelist)->if(List.tl aelist=[]) then diff_temp(List.hd aelist, str) else SUM(diff_temp(List.hd aelist,str)::diff_temp(SUM(List.tl aelist),str)::[])
  
  let diff (ae, str) =
  match ae with
  CONST(n)->CONST(0)
  | VAR(str1)->if(str1=str) then CONST(1) else CONST(0)
  | POWER(str1,n)->if(str1=str) then TIMES([CONST(n); POWER(str1, n-1)]) else CONST(0)
  | TIMES(aelist)->if(aelist=[]) then raise InvalidArgument
  else SUM(TIMES(diff_temp(List.hd aelist, str)::TIMES(List.tl aelist)::[])::TIMES(List.hd aelist::diff_temp(TIMES(List.tl aelist), str)::[])::[])
  | SUM(aelist)->if(aelist=[]) then raise InvalidArgument
  else SUM(diff_temp(List.hd aelist,str)::diff_temp(SUM(List.tl aelist),str)::[])