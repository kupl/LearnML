type nat = ZERO | SUCC of nat
exception Error of string


let rec natadd args = 
 match args with 
  (ZERO, n) -> n
  |(SUCC n, ZERO) -> (SUCC n)
  |(SUCC n1, SUCC n2) -> (natadd ((SUCC (SUCC n1)),
  				  (n2)))
;;

let rec natmul args = 
 match args with
  (ZERO,n) -> ZERO
  |(SUCC n, ZERO) -> ZERO
  |(SUCC n1, SUCC n2) -> (natadd ((SUCC n2),
  				  (natmul ((n1),
				  	   (SUCC n2)))))




(* test를 위한 코드 *)
(* intTonat : int -> nat  - int를 nat으로 변환 *)
let rec intTonat i = 
 if (i<0) then (raise (Error "Out of Range"))
 else (if (i=0)
       then (ZERO)
       else (SUCC (intTonat (i-1)))
      )
;;

(* natToint : nat -> int  - nat을 int로 변환 *)
let rec natToint n = 
 match n with
 ZERO -> 0
 |SUCC x -> (1 + (natToint x))
;;
