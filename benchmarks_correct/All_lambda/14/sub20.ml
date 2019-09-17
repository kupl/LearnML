(*********************************
 ** PL::HW[02].Problem[01]      **
 **                             **
 ** Mod. Init: 2014-09-25 19:16 **
 ** Mod. Fin.: 2014-09-25 21:24 **
 **                             **
 ** Writ. by : CMS              **
 *********************************)

type lambda = V of var
           | P of var * lambda
           | C of lambda * lambda
and var = string

let rec checkMap area_list target = 
	match target with 
	| V var -> 
		if List.mem var area_list
		then true
		else false
	| P (id, m) -> 
		if isName id
		then
			if List.mem id area_list
			then checkMap area_list m
			else checkMap (id::area_list) m
		else false
	| C (m1, m2) -> 
		if checkMap area_list m1
		then checkMap area_list m2
		else false
and isName str = 
	match str with
	| str1 -> true

let check = checkMap []

