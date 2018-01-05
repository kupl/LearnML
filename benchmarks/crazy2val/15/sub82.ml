type crazy2 = NIL | ZERO of crazy2 | ONE of crazy2 | MONE of crazy2
let rec helpeval : (crazy2*int*int) -> int = fun (c, ans, n) ->
	match c with
	| NIL -> ans
	| ZERO(c1) -> helpeval(c1,ans,n*2)
	| ONE(c1) -> helpeval(c1,ans+n,n*2)
	| MONE(c1) -> helpeval(c1,ans-n,n*2)
let crazy2val : crazy2 -> int = fun c -> helpeval(c,0,1)
(*
let a = ZERO(ONE(MONE(ZERO(MONE(ONE(ONE(NIL)))))))
let b = MONE(MONE(MONE(ONE(ZERO(MONE(MONE(ONE(ZERO(NIL)))))))))
let c = ONE(ZERO(ZERO(ONE(MONE(ONE(ONE(ZERO(MONE(ONE(NIL))))))))))
let d = MONE(MONE(ONE(ZERO(ZERO(ONE(ONE(ONE(NIL))))))))
let e = MONE(MONE(MONE(MONE(ONE(ONE(ONE(ONE(NIL))))))))
let a1=crazy2eval(a)
let b1=crazy2eval(b)
let c1=crazy2eval(c)
let d1=crazy2eval(d)
let e1=crazy2eval(e)
let _ = print_endline(string_of_bool (a1==78))
let _ = print_endline(string_of_bool (b1==33))
let _ = print_endline(string_of_bool (c1==345))
let _ = print_endline(string_of_bool (d1==225))
let _ = print_endline(string_of_bool (e1==225))*)
