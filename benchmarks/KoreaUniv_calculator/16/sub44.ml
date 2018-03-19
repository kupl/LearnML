
  type exp =
  | X
  | INT of int
  | ADD of exp * exp
  | SUB of exp * exp
  | MUL of exp * exp
  | DIV of exp * exp
  | SIGMA of exp * exp * exp

  let rec calculator : exp -> int
= fun ex -> match ex with
    | INT n -> n
    | ADD (a,b) -> calculator a + calculator b
    | SUB (a,b) -> calculator a + calculator b
    | MUL (a,b) -> calculator a * calculator b
    | DIV (a,b) -> calculator a / calculator b
    | SIGMA (a,b,c) -> si (calculator a) (calculator b) c
and si : int -> int -> exp -> int
= fun st fi ex -> match ex with
    | SIGMA (a,b,c) -> si (calculator a) (calculator b) c
    | _ -> if st = fi then ass ex st else if st > fi then ass ex fi + si st (fi+1) ex else ass ex st + si (st+1) fi  ex
and ass : exp -> int -> int
= fun ex num -> match ex with
    | X -> num
    | INT n -> n
    | ADD (a,b) -> ass a num + ass b num
    | SUB (a,b) -> ass a num - ass b num
    | MUL (a,b) -> ass a num * ass b num
    | DIV (a,b) -> ass a num / ass b num
    | SIGMA (a,b,c) -> si (calculator a) (calculator b) c