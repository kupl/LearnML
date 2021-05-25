exception TODO

type crazy2 =
  | NIL
  | ZERO of crazy2
  | ONE of crazy2
  | MONE of crazy2

let rec in_crazy2val (c: crazy2): int =
	match c with
	| NIL -> 0
	| (ZERO a) -> 2*(in_crazy2val a)
	| (ONE a) -> (2*(in_crazy2val a)+1)
	| (MONE a) -> (2*(in_crazy2val a)-1)

let sum_carry (a: int) (b: int) (c: int): int*int=
	let sum=a+b+c in
		(sum mod 2, sum/2)

let valTocrazy2 (a: int): crazy2=
	match a with
	| 0 -> (ZERO NIL)
	| 1 -> (ONE NIL)
	| -1 -> (MONE NIL)
	| _ -> NIL

let rec append_crazy2 (rhs: crazy2) (lhs: crazy2): crazy2=
	match rhs with
	| NIL -> lhs
	| (ZERO NIL)-> (ZERO lhs)
	| (ONE NIL) -> (ONE lhs)
	| (MONE NIL) -> (MONE lhs)
	| _ -> NIL

let divider (c: crazy2): crazy2*crazy2=
	match c with
	| NIL -> (NIL, NIL)
	| (ZERO a) -> ((ZERO NIL), a)
	| (ONE a) -> ((ONE NIL), a)
	| (MONE a) -> ((MONE NIL), a)

let sum_carry_as_crazy2 (lhs: crazy2) (rhs: crazy2) (c: crazy2): crazy2*crazy2=
	let lhs_num=in_crazy2val lhs in
	let rhs_num=in_crazy2val rhs in
	let c_num=in_crazy2val c in
	let sumXcarry=(sum_carry lhs_num rhs_num c_num) in
	let sum=(fst sumXcarry) in
	let carry=(snd sumXcarry) in
	(valTocrazy2(sum), valTocrazy2(carry))

let rec crazy2add_carry (lhs: crazy2) (rhs: crazy2) (carry: crazy2): crazy2=
	let lhs_first=(fst (divider lhs)) in
	let lhs_second=(snd (divider lhs)) in
	let rhs_first=(fst (divider rhs)) in
	let rhs_second=(snd (divider rhs)) in
	let sumXcarry=(sum_carry_as_crazy2 lhs_first rhs_first carry) in
	let sum=fst sumXcarry in
	let cry=snd sumXcarry in
	
	if ((lhs_first=NIL)&&(rhs_first=NIL)) then carry
	else (append_crazy2 sum (crazy2add_carry lhs_second rhs_second cry))


let rec crazy2add ((a: crazy2), (b: crazy2)): crazy2 =
	crazy2add_carry a b (ZERO NIL)


