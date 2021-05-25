type nat = ZERO | SUCC of nat;;

let check_ZERO n = if n=ZERO then true else false;;

let rec natadd n1 n2 = 
	if check_ZERO(n1) then n2
	else
	begin
	match n2 with 
		|SUCC(n2) -> SUCC(natadd n1 n2)
		|ZERO -> 
			begin
		 		let rec cal dummy = 
		 			if check_ZERO(dummy) then ZERO
		 			else
		 			match dummy with
		 			|SUCC(n2) -> SUCC (cal n2)
          			(*if check_ZERO(dummy) then ZERO
          			else (*if SUCC(b) then*) SUCC(cal b)*)
          			|ZERO -> ZERO
          			in cal n1
         		end
	end;;

 let rec natmul n1 n2 =
 	if check_ZERO(n1) then ZERO
	else if check_ZERO(n2) then ZERO
	else
	begin
 		match n2 with
 		|SUCC(n2) -> natadd n1 (natmul n1 n2)
 		|ZERO -> ZERO
 	end;;
