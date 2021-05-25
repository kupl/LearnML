type crazy2 = NIL | ZERO of crazy2 | ONE of crazy2 | MONE of crazy2

let rec crazy2add ((crz1:crazy2),(crz2:crazy2)) : crazy2 = 
match (crz1,crz2) with
| (NIL, cr2) -> cr2
| (cr1, NIL) -> cr1
| (ZERO NIL, cr2) -> cr2
| (cr1, ZERO NIL) -> cr1
| (ONE NIL, ONE NIL) -> ZERO(ONE NIL)
| (ONE NIL, MONE NIL) -> ZERO NIL
| (MONE NIL,ONE NIL) -> ZERO NIL
| (MONE NIL,MONE NIL)-> ZERO(MONE NIL)
| (ZERO cr1, ZERO (cr2)) -> ZERO(crazy2add(cr1,cr2))
| (ZERO cr1, ONE (cr2)) -> ONE(crazy2add(cr1,cr2))
| (ZERO cr1, MONE (cr2)) -> MONE(crazy2add(cr1,cr2))
| (ONE cr1, ZERO (cr2)) -> ONE(crazy2add(cr1,cr2))
| (MONE cr1, ZERO (cr2)) -> MONE(crazy2add(cr1,cr2))
| (ONE cr1, ONE cr2) -> ZERO(crazy2add(ONE NIL,(crazy2add(cr1,cr2))))
| (ONE cr1, MONE (cr2)) -> ZERO(crazy2add(cr1,cr2))
| (MONE cr1, ONE (cr2)) -> ZERO(crazy2add(cr1,cr2))
| (MONE cr1, MONE cr2) -> ZERO(crazy2add(MONE NIL,(crazy2add(cr1,cr2))))


(*
(*test old*)
let rec crazy2val (crz:crazy2) : int =
match crz with
| NIL -> 0 (*fix it*)
| ZERO NIL -> 0
| ONE NIL -> 1
| MONE NIL -> -1
| ZERO cr -> 0+((crazy2val cr)*2)
| ONE cr -> 1+((crazy2val cr)*2)
| MONE cr -> -1+((crazy2val cr)*2)
;; 

let _= 
let print_bool x = print_endline (string_of_bool x) in 

print_bool (0 = (crazy2val (crazy2add (ZERO NIL, ZERO NIL)))); 
print_bool (0 = (crazy2val (crazy2add (MONE NIL, ONE NIL)))); 
print_bool (1 = (crazy2val (crazy2add (ZERO NIL, ONE NIL)))); 
print_bool (4 = (crazy2val (crazy2add (ONE (ONE NIL), ONE NIL)))); 
print_bool (-683 = (crazy2val (crazy2add (MONE (ZERO (ZERO (ZERO NIL))), (ZERO (ONE (ONE (ZERO (MONE (ONE (ONE (ZERO (ONE (ZERO (MONE NIL))))))))))))))) 
;; 


(*test new*)


let mtwo = ZERO(ONE(MONE NIL)) 
let one = ONE(NIL) 
let five = ONE(ZERO(ONE NIL)) 
let mone = ONE(MONE NIL) 
let mnine = ONE(MONE(ZERO(MONE NIL))) 
let zero = ZERO(ZERO(ZERO(ZERO(ZERO(ZERO(ZERO(ZERO NIL))))))) 
let big1 = ZERO(ONE(ZERO(ONE(ZERO(MONE(MONE(ONE(MONE(ONE NIL))))))))) 
let big2 = ONE(MONE(MONE(ZERO(ONE(ZERO(MONE(MONE(ONE(MONE NIL))))))))) 

let _ = 
  let test_case : int * int * int -> unit = fun (n, x, y) -> 
      let result : int * int -> string = fun(x, y) -> 
            if(x == y) then "Pass" 
		          else "Failure -> " ^ string_of_int(x) ^ " vs " ^ string_of_int(y) in 
				      print_endline ("Case " ^ string_of_int(n) ^ " : " ^ result(x, y)) in 
				        test_case (1, crazy2val(crazy2add(mnine, mtwo)), crazy2val(crazy2add(mtwo, mnine))); 
					  test_case (2, crazy2val(mtwo) + crazy2val(mnine), crazy2val(crazy2add(mtwo, mnine))); 
					    test_case (3, crazy2val(five) + crazy2val(mnine), crazy2val(crazy2add(five, mnine))); 
					      test_case (4, crazy2val(crazy2add(mnine, one)) + crazy2val(five), crazy2val(mnine) + crazy2val(crazy2add(one, five))); 
					        test_case (5, crazy2val(crazy2add(crazy2add(mnine, mtwo), crazy2add(five, mtwo))), crazy2val(mnine) + crazy2val(mtwo) + crazy2val(five) + crazy2val(mtwo)); 
						  test_case (6, crazy2val(crazy2add(zero, mnine)), crazy2val(mnine)); 
						    test_case (7, crazy2val(crazy2add(big1, big2)), crazy2val(crazy2add(big2, big1))); 
						      test_case (8, crazy2val(crazy2add(big1, crazy2add(big1, big2))), crazy2val(crazy2add(crazy2add(big1, big1), big2)));
						      *)
