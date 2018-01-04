type crazy2 = NIL
            | ZERO of crazy2
            | ONE of crazy2
            | MONE of crazy2

let rec crazy2add ((x: crazy2), (y: crazy2)) : crazy2 =
     (match x with
      | NIL -> y
      | ZERO x' -> (match y with
                    | NIL -> x
                    | ZERO y' -> ZERO (crazy2add (x', y'))
                    | ONE y' -> ONE (crazy2add (x', y'))
                    | MONE y' -> MONE (crazy2add (x', y')))
      | ONE x' -> (match y with
                   | NIL -> x
                   | ZERO y' -> ONE (crazy2add (x', y'))
                   | ONE y' -> ZERO (crazy2add ((crazy2add (x', ONE NIL)), y'))
                   | MONE y' -> ZERO (crazy2add (x', y')))
      | MONE x' -> (match y with
                    | NIL -> x
                    | ZERO y' -> MONE (crazy2add (x', y'))
                    | ONE y' -> ZERO (crazy2add (x', y'))
                    | MONE y' -> ZERO (crazy2add ((crazy2add (x', MONE NIL)), y'))))
