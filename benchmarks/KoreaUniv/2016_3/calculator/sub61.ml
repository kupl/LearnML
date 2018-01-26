
  type exp =
  | X
  | INT of int
  | ADD of exp * exp
  | SUB of exp * exp
  | MUL of exp * exp
  | DIV of exp * exp
  | SIGMA of exp * exp * exp

  let calculator : exp -> int
  = fun exp -> 
      let castf d = Int64.shift_left (Int64.of_int d) 20 in
      let castd f = Int64.to_int (Int64.shift_right 
                      (Int64.add f (Int64.div (castf 1) 1000L)) 20) in
      let fone = castf 1 in
      let rec calc ep x = match ep with
      | X -> x
      | INT n -> castf n
      | ADD (e1, e2) -> Int64.add (calc e1 x) (calc e2 x)
      | SUB (e1, e2) -> Int64.sub (calc e1 x) (calc e2 x)
      | MUL (e1, e2) -> Int64.mul 
                          (Int64.shift_right (calc e1 x) 10)
                          (Int64.shift_right (calc e2 x) 10)
      | DIV (e1, e2) -> Int64.div
                          (Int64.shift_left  (calc e1 x) 10)
                          (Int64.shift_right (calc e2 x) 10)
      | SIGMA (ef, et, e) ->
          let (yf, yt) = (calc ef x, calc et x) in
          let (f, t) = if yf < yt then (yf, yt) else (yt, yf) in
          let rec loop y res = if y <= t 
            then loop (Int64.add y fone) (Int64.add res (calc e y)) 
            else res in 
          loop f 0L
      in castd (calc exp 0L)