type crazy2 = NIL | ZERO of crazy2 | ONE of crazy2 | MONE of crazy2
type help = ZE|ON|MON
let rec addhelp : crazy2*crazy2*help -> crazy2 = fun (ca,cb,h) ->
        match (ca,cb,h) with
        |(NIL,c0,_) | (c0,NIL,_) -> (match (c0,h) with
		|(NIL,ZE) -> NIL
		|(NIL,ON) -> ONE(NIL)
		|(NIL,MON) -> MONE(NIL)
		|(ZERO(c),ZE)|(ONE(c),MON)|(MONE(c),ON)
			-> ZERO(addhelp(NIL,c,ZE))
		|(ZERO(c),ON)|(ONE(c),ZE) -> ONE(addhelp(NIL,c,ZE))
		|(ONE(c),ON) -> ZERO(addhelp(NIL,c,ON))
		|(ZERO(c),MON)|(MONE(c),ZE) -> MONE(addhelp(NIL,c,ZE))
		|(MONE(c),MON) -> ZERO(addhelp(NIL,c,MON)))

	|(ZERO(c1),ZERO(c2),ZE)|(ZERO(c1),ONE(c2),MON)|(ZERO(c1),MONE(c2),ON)|(ONE(c1),ZERO(c2),MON)|(ONE(c1),MONE(c2),ZE)|(MONE(c1),ZERO(c2),ON)|(MONE(c1),ONE(c2),ZE) -> ZERO(addhelp(c1,c2,ZE))
	|(ZERO(c1),ZERO(c2),ON)|(ZERO(c1),ONE(c2),ZE)|(ONE(c1),ZERO(c2),ZE)|(ONE(c1),ONE(c2),MON)|(ONE(c1),MONE(c2),ON)|(MONE(c1),ONE(c2),ON) -> ONE(addhelp(c1,c2,ZE))
	|(ONE(c1),ONE(c2),ZE)|(ONE(c1),ZERO(c2),ON)|(ZERO(c1),ONE(c2),ON) -> ZERO(addhelp(c1,c2,ON))
	|(ONE(c1),ONE(c2),ON) -> ONE(addhelp(c1,c2,ON))
	|(ZERO(c1),ZERO(c2),MON)|(ZERO(c1),MONE(c2),ZE)|(MONE(c1),ZERO(c2),ZE)|(MONE(c1),MONE(c2),ON)|(MONE(c1),ONE(c2),MON)|(ONE(c1),MONE(c2),MON) -> MONE(addhelp(c1,c2,ZE))
	|(MONE(c1),MONE(c2),ZE)|(MONE(c1),ZERO(c2),MON)|(ZERO(c1),MONE(c2),MON) -> ZERO(addhelp(c1,c2,MON))
	|(MONE(c1),MONE(c2),MON) -> MONE(addhelp(c1,c2,MON))


let crazy2add : crazy2 * crazy2 -> crazy2 = fun (c1,c2)->addhelp(c1,c2,ZE)

(*
let rec helpeval : (crazy2*int*int) -> int = fun (c, ans, n) ->
        match c with
        | NIL -> ans
        | ZERO(c1) -> helpeval(c1,ans,n*2)
        | ONE(c1) -> helpeval(c1,ans+n,n*2)
        | MONE(c1) -> helpeval(c1,ans-n,n*2)
let crazy2eval : crazy2 -> int = fun c -> helpeval(c,0,1)
let a = ZERO(ONE(MONE(ZERO(MONE(ONE(ONE(NIL)))))))
let b = MONE(MONE(MONE(ONE(ZERO(MONE(MONE(ONE(ZERO(NIL)))))))))
let c = ONE(ZERO(ZERO(ONE(MONE(ONE(ONE(ZERO(MONE(ONE(NIL))))))))))
let d = MONE(MONE(ONE(ZERO(ZERO(ONE(ONE(ONE(NIL))))))))
let e = MONE(MONE(MONE(MONE(ONE(ONE(ONE(ONE(NIL))))))))
let _ = print_endline(string_of_bool (crazy2eval(a)+crazy2eval(b)==crazy2eval(crazy2add(a,b))))
let _ = print_endline(string_of_bool (crazy2eval(a)+crazy2eval(c)==crazy2eval(crazy2add(a,c))))
let _ = print_endline(string_of_bool (crazy2eval(a)+crazy2eval(d)==crazy2eval(crazy2add(a,d))))
let _ = print_endline(string_of_bool (crazy2eval(c)+crazy2eval(b)==crazy2eval(crazy2add(b,c))))
let _ = print_endline(string_of_bool (crazy2eval(d)+crazy2eval(b)==crazy2eval(crazy2add(d,b))))
let _ = print_endline(string_of_bool (crazy2eval(d)+crazy2eval(c)==crazy2eval(crazy2add(c,d))))
let _ = print_endline(string_of_bool (crazy2eval(a)+crazy2eval(e)==crazy2eval(crazy2add(a,e))))
let _ = print_endline(string_of_bool (crazy2eval(e)+crazy2eval(b)==crazy2eval(crazy2add(b,e))))
let _ = print_endline(string_of_bool (crazy2eval(d)+crazy2eval(e)==crazy2eval(crazy2add(d,e))))
let _ = print_endline(string_of_bool (crazy2eval(e)+crazy2eval(c)==crazy2eval(crazy2add(c,e))))
*)
