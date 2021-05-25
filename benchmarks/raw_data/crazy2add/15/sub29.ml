(* C:\Users\saigoy\Desktop\crazy2add.ml *)

type crazy2 = NIL
  | ZERO of crazy2
  | ONE of crazy2
  | MONE of crazy2;;

let rec crazy2add : crazy2 * crazy2 -> crazy2 = fun (lc , rc) ->
  	let crazy2val_single c = 
  		match c with
  		| NIL -> 0| ZERO c -> 0
  		| ONE c -> 1
  		| MONE c -> -1	in

  	let crazy2_sum sum = 
    	if ( sum == 1 || sum == 3) then 1
  		else if ( sum == -1 || sum == -3 ) then -1
  		else 0	in

  	let crazy2_carry sum = 
 		if ( sum == 2 || sum == 3 ) then 1
  		else if ( sum == -2 || sum == -3 ) then -1
  		else 0	in

	let crazy2_tail c = 
		match c with
		| NIL -> NIL
		| ZERO ctl -> ctl
		| ONE ctl -> ctl
		| MONE ctl -> ctl	in  	

	let rec crazy2add_with_carry (lc, rc, carry) =  
  		let sum = (crazy2val_single lc) + (crazy2val_single rc) + carry in
		let c_sum = crazy2_sum sum in
		let c_carry = crazy2_carry sum in
		let l_tail = crazy2_tail lc in
		let r_tail = crazy2_tail rc in
  	if ( lc == NIL && rc == NIL && carry == 0) then NIL
	else if (c_sum == 0) then ZERO ( crazy2add_with_carry (l_tail, r_tail, c_carry) ) 
	else if (c_sum == 1) then ONE ( crazy2add_with_carry (l_tail, r_tail, c_carry) )
	else MONE ( crazy2add_with_carry (l_tail, r_tail, c_carry) ) in

crazy2add_with_carry(lc, rc, 0);;