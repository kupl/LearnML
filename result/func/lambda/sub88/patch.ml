type lambda = V of var | P of (var * lambda) | C of (lambda * lambda)

and var = string

let rec isexist (vars : string list) (v : string) : bool =
  match vars with
  | [] -> false
  | hd :: tl -> if hd = v then true else isexist tl v


let rec chkvars (lambda : lambda) (vars : string list) : bool =
  match lambda with
  | V v -> isexist vars v
  | P (v, e) -> chkvars e (v :: vars)
  | C (__s13, __s14) -> chkvars __s14 vars && chkvars __s13 vars
  | C (V v, e) -> chkvars e (v :: vars)


let check (lambda : lambda) : bool = chkvars lambda []
