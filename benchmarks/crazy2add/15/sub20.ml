type crazy2 = NIL | ZERO of crazy2 | ONE of crazy2 | MONE of crazy2 


let crazy2add ((c1:crazy2), (c2:crazy2)) :crazy2 =
(* Easy Way to do it.
(let rec int2crazy2 (i:int) :crazy2 = 
  (let q, r = i / 2, i mod 2 in
    if q = 0 then (match r with -1 -> MONE NIL | 1 -> ONE NIL | _ -> ZERO NIL)
	else let n = int2crazy2 q in (match r with -1 -> MONE n | 1 -> ONE n | _ -> ZERO n))
  in
  let rec crazy2val (c :crazy2) :int = 
	(match c with
	  NIL -> 0
	| ZERO next -> 2 * (crazy2val next)
	| ONE next -> 1 + 2 * (crazy2val next)
	| MONE next -> -1 + 2 * (crazy2val next))
  in
  int2crazy2 ((crazy2val c1)+(crazy2val c2)))
*)
  let rec c2addWithCarry x y c =
  match x, y, c with
  | _, _, ZERO _ -> c2addWithCarry x y NIL
  | NIL, NIL, n | NIL, n, NIL | n, NIL, NIL -> n
  | NIL, n, c | n, NIL, c -> 
    (match n, c with
	 | n , (NIL|ZERO _) -> n
	 | NIL, c -> c
	 | MONE k, MONE _ -> ZERO (c2addWithCarry NIL k (MONE NIL))
	 | ZERO k, MONE _ -> MONE k
	 | MONE k, ONE _ | ONE k, MONE _ -> ZERO k
	 | ZERO k, ONE _ -> ONE k
	 | ONE k, ONE _ -> ZERO (c2addWithCarry NIL k (ONE NIL)))
  | MONE n1, MONE n2, MONE _ -> MONE (c2addWithCarry n1 n2 (MONE NIL))
  | MONE n1, MONE n2, NIL | MONE n1, ZERO n2, MONE _ | ZERO n1, MONE n2, MONE _
    -> ZERO (c2addWithCarry n1 n2 (MONE NIL))
  | MONE n1, ZERO n2, NIL | ZERO n1, MONE n2, NIL | ZERO n1, ZERO n2, MONE _ 
  | MONE n1, MONE n2, ONE _ | MONE n1, ONE n2, MONE _ | ONE n1, MONE n2, MONE _
    -> MONE (c2addWithCarry n1 n2 (ZERO NIL))
  | ZERO n1, ZERO n2, NIL
  | MONE n1, ONE n2, NIL | MONE n1, ZERO n2, ONE _ | ZERO n1, MONE n2, ONE _
  | ONE n1, MONE n2, NIL | ONE n1, ZERO n2, MONE _ | ZERO n1, ONE n2, MONE _
    -> ZERO (c2addWithCarry n1 n2 (ZERO NIL))
  | ONE n1, ZERO n2, NIL | ZERO n1, ONE n2, NIL | ZERO n1, ZERO n2, ONE _ 
  | MONE n1, ONE n2, ONE _ | ONE n1, ONE n2, MONE _ | ONE n1, MONE n2, ONE _
    -> ONE (c2addWithCarry n1 n2 (ZERO NIL))
  | ONE n1, ONE n2, NIL | ONE n1, ZERO n2, ONE _ | ZERO n1, ONE n2, ONE _
    -> ZERO (c2addWithCarry n1 n2 (ONE NIL))
  | ONE n1, ONE n2, ONE _ -> ONE (c2addWithCarry n1 n2 (ONE NIL))
  in c2addWithCarry c1 c2 NIL
  
(* TESTCASE
let rec string_of_crazy2 c = 
  match c with
  | NIL -> ""
  | ZERO n -> "0" ^ (string_of_crazy2 n)
  | MONE n -> "-" ^ (string_of_crazy2 n)
  | ONE n  -> "+" ^ (string_of_crazy2 n)

let rec int2crazy2_test (i:int) :crazy2 = 
  (let q, r = i / 2, i mod 2 in
    if q = 0 then (match r with -1 -> MONE NIL | 1 -> ONE NIL | _ -> ZERO NIL)
	else let n = int2crazy2_test q in (match r with -1 -> MONE n | 1 -> ONE n | _ -> ZERO n))

let rec crazy2val_test (c :crazy2) :int = 
	(match c with
	  NIL -> 0
	| ZERO next -> 2 * (crazy2val_test next)
	| ONE next -> 1 + 2 * (crazy2val_test next)
	| MONE next -> -1 + 2 * (crazy2val_test next))

let testA = int2crazy2_test 1095
let testB = int2crazy2_test (-1095)
let testC = crazy2add (testA, testB)

let _ = print_endline((string_of_crazy2 testA) ^ " + " ^ (string_of_crazy2 testB) ^ " = " ^ (string_of_crazy2 testC))
let _ = print_endline(string_of_int (crazy2val_test testC))
*)
