(*********************************
 ** PL::HW[02].Problem[01]      **
 **                             **
 ** Mod. Init: 2014-09-25 19:16 **
 ** Mod. Fin.: 2014-09-25 21:24 **
 **                             **
 ** Writ. by : CMS              **
 *********************************)

type metro = STATION of name
           | AREA of name * metro
           | CONNECT of metro * metro
and name = string

let rec checkMetroMap area_list target = 
	match target with 
	| STATION name -> 
		if List.mem name area_list
		then true
		else false
	| AREA (id, m) -> 
		if isName id
		then
			if List.mem id area_list
			then checkMetroMap area_list m
			else checkMetroMap (id::area_list) m
		else false
	| CONNECT (m1, m2) -> 
		if checkMetroMap area_list m1
		then checkMetroMap area_list m2
		else false
and isName str = 
	match str with
	| str1 -> true

let checkMetro = checkMetroMap []

