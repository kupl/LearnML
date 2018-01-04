type crazy2 = NIL | ZERO of crazy2 | ONE of crazy2 | MONE of crazy2

let rec crazy2add : crazy2 * crazy2 -> crazy2 = fun (x, y) -> 
  let value_first x = match x with
  | NIL -> 0
  | ZERO _ -> 0
  | ONE _ -> 1
  | MONE _ -> -1
  in let tail x = match x with
  | NIL -> NIL
  | ZERO x' -> x'
  | ONE x' -> x'
  | MONE x' -> x'
  in let rec rev x r = match x with
  | NIL -> r
  | ZERO x' -> rev x' (ZERO r)
  | ONE x' -> rev x' (ONE r)
  | MONE x' -> rev x' (MONE r)
  in (
    let rec crazy2add_temp x y c r = if (x, y, c) = (NIL, NIL, 0) then r
    else match ((value_first x) + (value_first y) + c) with
    | -3 -> crazy2add_temp (tail x) (tail y) (-1) (MONE r)
    | -2 -> crazy2add_temp (tail x) (tail y) (-1) (ZERO r)
    | -1 -> crazy2add_temp (tail x) (tail y) 0 (MONE r)
    | 0 -> crazy2add_temp (tail x) (tail y) 0 (ZERO r)
    | 1 -> crazy2add_temp (tail x) (tail y) 0 (ONE r)
    | 2 -> crazy2add_temp (tail x) (tail y) 1 (ZERO r)
    | 3 -> crazy2add_temp (tail x) (tail y) 1 (ONE r)
    | _ -> failwith "ERROR!"
    in rev (crazy2add_temp x y 0 NIL) NIL
  )
