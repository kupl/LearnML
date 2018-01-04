type nat = ZERO | SUCC of nat

let rec natadd = function (x, y) -> let dec x = match x with
					SUCC y -> y 
					|ZERO -> ZERO in 

						match (x,y) with
  					(ZERO, _) -> y
  					|(_, ZERO) -> x
					|(_,_) -> SUCC (natadd (x, dec y))

let rec natmul = function (x, y) -> let dec x = match x with
					SUCC y -> y 
					|ZERO -> ZERO in
					
					let rec natadd = function (x, y) -> match (x,y) with
  					(ZERO, _) -> y
  					|(_, ZERO) -> x
					|(_,_) -> SUCC (natadd (x, dec y)) in

					match (x,y) with
  					(ZERO, _) -> ZERO
  					|(_, ZERO) -> ZERO
					|(_,_) -> natadd (x, natmul(x, dec y));;