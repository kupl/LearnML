type crazy2 = NIL
            | ZERO of crazy2
            | ONE of crazy2
            | MONE of crazy2

let crazy2add ((c1: crazy2), (c2: crazy2)) : crazy2 =
  let rec op1 (a, b) =
    let rec op2 (a, b, c) =
      match (a, b, c) with
      | (NIL, NIL, 1) -> ONE NIL
      | (NIL, NIL, -1) -> MONE NIL
      | (NIL, ZERO b', 1) -> ONE b'
      | (NIL, ONE b', 1) -> ZERO (op1 (b', ONE NIL))
      | (NIL, MONE b', 1) -> ZERO b'
      | (NIL, ZERO b', -1) -> MONE b'
      | (NIL, ONE b', -1) -> ZERO b'
      | (NIL, MONE b', -1) -> ZERO (op1 (b', MONE NIL))
      | (ZERO a', NIL, 1) -> ONE a'
      | (ZERO a', ZERO b', 1) -> ONE (op1 (a', b'))
      | (ZERO a', ONE b', 1) -> ZERO (op2 (a', b', 1))
      | (ZERO a', MONE b', 1) -> ZERO (op1 (a', b'))
      | (ZERO a', NIL, -1) -> MONE a'
      | (ZERO a', ZERO b', -1) -> MONE (op1 (a', b'))
      | (ZERO a', ONE b', -1) -> ZERO (op1 (a', b'))
      | (ZERO a', MONE b', -1) -> ZERO (op2 (a', b', -1))
      | (ONE a', NIL, 1) -> ZERO (op1 (a', ONE NIL))
      | (ONE a', ZERO b', 1) -> ZERO (op2 (a', b',1 ))
      | (ONE a', ONE b', 1) -> ONE (op2 (a', b', 1))
      | (ONE a', MONE b', 1) -> ONE (op1 (a', b'))
      | (ONE a', NIL, -1) -> ZERO a'
      | (ONE a', ZERO b', -1) -> ZERO (op1 (a', b'))
      | (ONE a', ONE b', -1) -> ONE (op1 (a', b'))
      | (ONE a', MONE b', -1) -> MONE (op1 (a', b'))
      | (MONE a', NIL, 1) -> ZERO a'
      | (MONE a', ZERO b', 1) -> ZERO (op1 (a', b'))
      | (MONE a', ONE b', 1) -> ONE (op1 (a', b'))
      | (MONE a', MONE b', 1) -> MONE (op1 (a', b'))
      | (MONE a', NIL, -1) -> ZERO (op1 (a', MONE NIL))
      | (MONE a', ZERO b', -1) -> ZERO (op2 (a', b', -1))
      | (MONE a', ONE b', -1) -> MONE (op1 (a', b'))
      | (MONE a', MONE b', -1) -> MONE (op2 (a', b', -1))
      | (_, _, _) -> op1(a, b)
    in
    match (a, b) with
    | (NIL, NIL) -> NIL
    | (NIL, _) -> b
    | (_, NIL) -> a
    | (ZERO a', ZERO b') -> ZERO (op1 (a', b'))
    | (ZERO a', ONE b') -> ONE (op1 (a', b'))
    | (ZERO a', MONE b') -> MONE (op1 (a', b'))
    | (ONE a', ZERO b') -> ONE (op1 (a', b'))
    | (MONE a', ZERO b') -> MONE (op1 (a', b'))
    | (ONE a', ONE b') -> ZERO (op2 (a', b', 1))
    | (ONE a', MONE b') -> ZERO (op1 (a', b'))
    | (MONE a', ONE b') -> ZERO (op1 (a', b'))
    | (MONE a', MONE b') -> ZERO (op2 (a', b', -1))
  in
  op1 (c1, c2)