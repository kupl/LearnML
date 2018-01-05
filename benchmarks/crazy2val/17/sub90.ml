type crazy2= NIL
			 |ZERO of crazy2
			 |ONE of crazy2
			 |MONE of crazy2

let rec power num expo = match expo with
  							 0 -> 1
  							| 1 -> num
  							| any -> let x = power num (any/2) in
   								 x*x* (if any mod 2 = 0 then 1 else num)

let rec cval ((crz:crazy2),(i:int)) : int = match crz with
									NIL-> 0
									|ZERO x-> cval (x,i+1)
									|ONE x-> 1*(power 2 i)+ cval (x,i+1)
									|MONE x-> (-1)*(power 2 i)+ cval (x,i+1)

let crazy2val (crz:crazy2) : int = cval (crz,0)


