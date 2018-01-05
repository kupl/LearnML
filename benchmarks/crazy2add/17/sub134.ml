(*
 * 2017 - 09 - 22
 * PL Homework 2-3
 * Joonmo Yang
*)

type crazy2 = NIL | ZERO of crazy2 | ONE of crazy2 | MONE of crazy2

let crazy2val : crazy2 -> int =
  fun crz ->
  (
    let rec crazy2val_sub : crazy2 * int * int -> int =
      fun (c2, sum, dgr) ->
        ( match c2 with
          | NIL -> sum
          | ZERO c -> crazy2val_sub(c, sum, dgr * 2)
          | ONE c -> crazy2val_sub(c, sum + dgr, dgr * 2)
          | MONE c -> crazy2val_sub(c, sum - dgr, dgr * 2)
        ) 
    in crazy2val_sub(crz, 0 , 1)
  )

(* make reverse crazy2. *)
let rec rev_crz : crazy2 * crazy2 -> crazy2 = 
  fun (crz, rst) ->
  ( match crz with
    | NIL -> rst
    | ZERO c -> rev_crz(c, ZERO rst)
    | ONE c -> rev_crz(c, ONE rst)
    | MONE c -> rev_crz(c, MONE rst)
  )

let crazy2add : crazy2 * crazy2 -> crazy2 =
  fun (crz1, crz2) ->
  (
    let rec crazy2add_sub : crazy2 * crazy2 * crazy2 * crazy2 -> crazy2 =
      fun (c1, c2, crry, csum) ->
        ( match c1, c2 with
          | NIL, NIL -> 
            ( match crry with
              | NIL -> csum 
              | ZERO _ -> ZERO csum 
              | ONE _ -> ONE csum 
              | MONE _ -> MONE csum
            )
          | NIL, ZERO c -> 
            ( match crry with
              | NIL -> crazy2add_sub(NIL, c, NIL, ZERO csum)
              | ZERO _ -> crazy2add_sub(NIL, c, NIL, ZERO csum)
              | ONE _ -> crazy2add_sub(NIL, c, NIL, ONE csum) 
              | MONE _ -> crazy2add_sub(NIL, c, NIL, MONE csum)
            )
          | NIL, ONE c ->
            ( match crry with
              | NIL -> crazy2add_sub(NIL, c, NIL, ONE csum)
              | ZERO _ -> crazy2add_sub(NIL, c, NIL, ONE csum)
              | ONE _ -> crazy2add_sub(NIL, c, ONE NIL, ZERO csum) 
              | MONE _ -> crazy2add_sub(NIL, c, NIL, ZERO csum)
            )
          | NIL, MONE c ->
            ( match crry with
              | NIL -> crazy2add_sub(NIL, c, NIL, MONE csum)
              | ZERO _ -> crazy2add_sub(NIL, c, NIL, MONE csum)
              | ONE _ -> crazy2add_sub(NIL, c, NIL, ZERO csum) 
              | MONE _ -> crazy2add_sub(NIL, c, MONE NIL, ZERO csum)
            )
          | ZERO c1z, NIL -> 
            ( match crry with
              | NIL -> crazy2add_sub(c1z, NIL, NIL, ZERO csum)
              | ZERO _ -> crazy2add_sub(c1z, NIL, NIL, ZERO csum)
              | ONE _ -> crazy2add_sub(c1z, NIL, NIL, ONE csum) 
              | MONE _ -> crazy2add_sub(c1z, NIL, NIL, MONE csum)
            )
          | ZERO c1z, ZERO c2z ->
            ( match crry with
              | NIL -> crazy2add_sub(c1z, c2z, NIL, ZERO csum)
              | ZERO _ -> crazy2add_sub(c1z, c2z, NIL, ZERO csum)
              | ONE _ -> crazy2add_sub(c1z, c2z, NIL, ONE csum) 
              | MONE _ -> crazy2add_sub(c1z, c2z, NIL, MONE csum)
            )
          | ZERO c1z, ONE c2o ->
            ( match crry with
              | NIL -> crazy2add_sub(c1z, c2o, NIL, ONE csum)
              | ZERO _ -> crazy2add_sub(c1z, c2o, NIL, ONE csum)
              | ONE _ -> crazy2add_sub(c1z, c2o, ONE NIL, ZERO csum) 
              | MONE _ -> crazy2add_sub(c1z, c2o, NIL, ZERO csum)
            )
          | ZERO c1z, MONE c2m ->
            ( match crry with
              | NIL -> crazy2add_sub(c1z, c2m, NIL, MONE csum)
              | ZERO _ -> crazy2add_sub(c1z, c2m, NIL, MONE csum)
              | ONE _ -> crazy2add_sub(c1z, c2m, NIL, ZERO csum) 
              | MONE _ -> crazy2add_sub(c1z, c2m, MONE NIL, ZERO csum)
            )
          | ONE c1o, NIL ->
            ( match crry with
              | NIL -> crazy2add_sub(c1o, NIL, NIL, ONE csum)
              | ZERO _ -> crazy2add_sub(c1o, NIL, NIL, ONE csum)
              | ONE _ -> crazy2add_sub(c1o, NIL, ONE NIL, ZERO csum) 
              | MONE _ -> crazy2add_sub(c1o, NIL, NIL, ZERO csum)
            )
          | ONE c1o, ZERO c2z -> 
            ( match crry with
              | NIL -> crazy2add_sub(c1o, c2z, NIL, ONE csum)
              | ZERO _ -> crazy2add_sub(c1o, c2z, NIL, ONE csum)
              | ONE _ -> crazy2add_sub(c1o, c2z, ONE NIL, ZERO csum) 
              | MONE _ -> crazy2add_sub(c1o, c2z, NIL, ZERO csum)
            )
          | ONE c1o, ONE c2o ->
            ( match crry with
              | NIL -> crazy2add_sub(c1o, c2o, ONE NIL, ZERO csum)
              | ZERO _ -> crazy2add_sub(c1o, c2o, ONE NIL, ZERO csum)
              | ONE _ -> crazy2add_sub(c1o, c2o, ONE NIL, ONE csum) 
              | MONE _ -> crazy2add_sub(c1o, c2o, NIL, ZERO csum)
            )
          | ONE c1o, MONE c2m ->
            ( match crry with
              | NIL -> crazy2add_sub(c1o, c2m, NIL, ZERO csum)
              | ZERO _ -> crazy2add_sub(c1o, c2m, NIL, ZERO csum)
              | ONE _ -> crazy2add_sub(c1o, c2m, NIL, ONE csum) 
              | MONE _ -> crazy2add_sub(c1o, c2m, NIL, MONE csum)
            )
          | MONE c1m, NIL ->
            ( match crry with
              | NIL -> crazy2add_sub(c1m, NIL, NIL, MONE csum)
              | ZERO _ -> crazy2add_sub(c1m, NIL, NIL, MONE csum)
              | ONE _ -> crazy2add_sub(c1m, NIL, NIL, ZERO csum) 
              | MONE _ -> crazy2add_sub(c1m, NIL, MONE NIL, ZERO csum)
            )
          | MONE c1m, ZERO c2z ->
            ( match crry with
              | NIL -> crazy2add_sub(c1m, c2z, NIL, MONE csum)
              | ZERO _ -> crazy2add_sub(c1m, c2z, NIL, MONE csum)
              | ONE _ -> crazy2add_sub(c1m, c2z, NIL, ZERO csum) 
              | MONE _ -> crazy2add_sub(c1m, c2z, MONE NIL, ZERO csum)
            )
          | MONE c1m, ONE c2o ->
            ( match crry with
              | NIL -> crazy2add_sub(c1m, c2o, NIL, ZERO csum)
              | ZERO _ -> crazy2add_sub(c1m, c2o, NIL, ZERO csum)
              | ONE _ -> crazy2add_sub(c1m, c2o, NIL, ONE csum) 
              | MONE _ -> crazy2add_sub(c1m, c2o, NIL, MONE csum)
            )
          | MONE c1m, MONE c2m ->
            ( match crry with
              | NIL -> crazy2add_sub(c1m, c2m, MONE NIL, ZERO csum)
              | ZERO _ -> crazy2add_sub(c1m, c2m, MONE NIL, ZERO csum)
              | ONE _ -> crazy2add_sub(c1m, c2m, NIL, MONE csum) 
              | MONE _ -> crazy2add_sub(c1m, c2m, MONE NIL, MONE csum)
            ) 
        )
    in rev_crz(crazy2add_sub(crz1, crz2, NIL, NIL), NIL)
  )

(* test cases
let c1 = ONE NIL
let c2 = ONE (ZERO (ONE NIL))
let c3 = ONE (MONE NIL)
let c4 = ONE (MONE (ZERO (MONE NIL)))
let c5 = MONE (ONE (ZERO (ONE NIL)))
let c6 = ONE (ONE (ONE (ONE (ONE NIL))))

let c3c4 = crazy2add(c3, c4)
let c5c6 = crazy2add(c5, c6)

let b1 = ((crazy2val (crazy2add(c3, c4))) = crazy2val(c3) + crazy2val(c4))
let b2 = ((crazy2val (crazy2add(c4, c5))) = crazy2val(c4) + crazy2val(c5))
let b3 = ((crazy2val (crazy2add(c5, c6))) = crazy2val(c5) + crazy2val(c6))

let mtwo = ZERO(ONE(MONE NIL)) 
let one = ONE(NIL) 
let five = ONE(ZERO(ONE NIL)) 
let mone = ONE(MONE NIL) 
let mnine = ONE(MONE(ZERO(MONE NIL))) 
let zero = ZERO(ZERO(ZERO(ZERO(ZERO(ZERO(ZERO(ZERO NIL))))))) 
let five2 = ONE(ZERO(ONE(ZERO(ZERO(ZERO(ZERO(ZERO NIL))))))) 
let big1 = ZERO(ONE(ZERO(ONE(ZERO(MONE(MONE(ONE(MONE(ONE NIL))))))))) 
let big2 = ONE(MONE(MONE(ZERO(ONE(ZERO(MONE(MONE(ONE(MONE NIL))))))))) 

let _ = 
  let test_case : int * int * int -> unit = fun (n, x, y) -> 
    let result : int * int -> string = fun(x, y) -> 
      if(x == y) then "Pass" 
      else "Failure -> " ^ string_of_int(x) ^ " vs " ^ string_of_int(y) in 
    print_endline ("Case " ^ string_of_int(n) ^ " : " ^ result(x, y)) in 
  let test_nocrazy2val : bool -> unit = fun x -> 
    let detector = fun x -> if(x = true) then "Pass" else "crazy2val detected" in 
    print_endline ("crazy2val detector : " ^ detector(x)) in 
  test_nocrazy2val (crazy2add(zero, five) = five2); 
  test_case (1, crazy2val(crazy2add(mnine, mtwo)), crazy2val(crazy2add(mtwo, mnine))); 
  test_case (2, crazy2val(mtwo) + crazy2val(mnine), crazy2val(crazy2add(mtwo, mnine))); 
  test_case (3, crazy2val(five) + crazy2val(mnine), crazy2val(crazy2add(five, mnine))); 
  test_case (4, crazy2val(crazy2add(mnine, one)) + crazy2val(five), crazy2val(mnine) + crazy2val(crazy2add(one, five))); 
  test_case (5, crazy2val(crazy2add(crazy2add(mnine, mtwo), crazy2add(five, mtwo))), crazy2val(mnine) + crazy2val(mtwo) + crazy2val(five) + crazy2val(mtwo)); 
  test_case (6, crazy2val(crazy2add(zero, mnine)), crazy2val(mnine)); 
  test_case (7, crazy2val(crazy2add(big1, big2)), crazy2val(crazy2add(big2, big1))); 
*)
