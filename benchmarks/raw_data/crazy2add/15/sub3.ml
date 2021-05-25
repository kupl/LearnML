(*
    PL 2-2
    2008-11609 박성원
*)

type crazy2 = NIL | ZERO of crazy2 | ONE of crazy2 | MONE of crazy2

let crazy2add (c1, c2) =
  let getNext c =
    match c with
    | NIL -> NIL
    | ZERO n -> n
    | ONE n -> n
    | MONE n -> n
  in
  let calcCarry c1 c2 carry =
    match (c1, c2, carry) with
    | (ONE _,  ONE _,  ONE _)
    | (ONE _,  ONE _,  ZERO _)
    | (ONE _,  ZERO _, ONE _)
    | (ZERO _, ONE _,  ONE _)  -> ONE NIL
    | (MONE _, MONE _, MONE _)
    | (MONE _, MONE _, ZERO _)
    | (MONE _, ZERO _, MONE _)
    | (ZERO _, MONE _, MONE _) -> MONE NIL
    | _                        -> ZERO NIL
  in
  let rec addImpl c1 c2 carry =
    match (c1, c2, carry) with
    | (NIL, NIL, ZERO _) -> NIL
    | (NIL, NIL, _) -> carry
    | _ ->
      let p1 = if c1 = NIL then ZERO NIL else c1 in
      let p2 = if c2 = NIL then ZERO NIL else c2 in
      let nextCalc = addImpl (getNext p1) (getNext p2) (calcCarry p1 p2 carry) in
      let nextVal = if nextCalc = ZERO NIL then NIL else nextCalc in
      match (p1, p2, carry) with
      | (ONE _,  MONE _, ONE _)  -> ONE  nextVal
      | (ONE _,  MONE _, ZERO _) -> ZERO nextVal
      | (ONE _,  MONE _, MONE _) -> MONE nextVal
      | (ZERO _, ZERO _, ONE _)  -> ONE  nextVal
      | (ZERO _, ZERO _, ZERO _) -> ZERO nextVal
      | (ZERO _, ZERO _, MONE _) -> MONE nextVal
      | (MONE _, ONE _,  ONE _)  -> ONE  nextVal
      | (MONE _, ONE _,  ZERO _) -> ZERO nextVal
      | (MONE _, ONE _,  MONE _) -> MONE nextVal

      | (ONE _,  ONE _,  ZERO _) -> ZERO nextVal
      | (ONE _,  ONE _,  _)      -> ONE  nextVal
      | (ONE _,  ZERO _, ZERO _) -> ONE  nextVal
      | (ONE _,  ZERO _, _)      -> ZERO nextVal
      | (ZERO _, ONE _,  ZERO _) -> ONE  nextVal
      | (ZERO _, ONE _,  _)      -> ZERO nextVal
      | (ZERO _, MONE _, ZERO _) -> MONE nextVal
      | (ZERO _, MONE _, _)      -> ZERO nextVal
      | (MONE _, ZERO _, ZERO _) -> MONE nextVal
      | (MONE _, ZERO _, _)      -> ZERO nextVal
      | (MONE _, MONE _, ZERO _) -> ZERO nextVal
      | (MONE _, MONE _, _)      -> MONE nextVal

      | _                        -> ZERO nextVal
  in
  addImpl c1 c2 (ZERO NIL)
