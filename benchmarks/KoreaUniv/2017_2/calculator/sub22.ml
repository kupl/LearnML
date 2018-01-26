  (*problem 5*)
  type exp = X
    | INT of int
    | ADD of exp*exp
    | SUB of exp*exp
    | MUL of exp*exp
    | DIV of exp*exp
    | SIGMA of exp*exp*exp

  let calculator : exp -> int
  = fun e -> let rec calc = fun (e,i) -> match e with
  | X -> i
  | INT a -> a
  | ADD (a,b) -> calc(a,i)+calc(b,i)
  | SUB (a,b) -> calc(a,i)-calc(b,i)
  | MUL (a,b) -> calc(a,i)*calc(b,i)
  | DIV (a,b) -> calc(a,i)/calc(b,i) in
  let rec calcsigma = fun (a,b,c,n) -> if b=n then calc(c,n)
  else calc(c,n)+calcsigma(a,b,c,n+1) in
  let getsigma = fun e -> match e with
  | SIGMA (INT a,INT b,c) -> calcsigma(a,b,c,a) in getsigma e;;

  (* problem 6*)
  type mobile = branch * branch
  and branch = SimpleBranch of length * weight
  | CompoundBranch of length * mobile
  and length = int
  and weight = int

  let balanced : mobile -> bool
  = fun m -> let rec getw = fun b -> match b with
  | SimpleBranch (_,w) -> w
  | CompoundBranch (_,bc) -> (match bc with
      | (b1,b2) -> (getw(b1))+(getw(b2))) in
  let getl = fun b -> match b with
  | SimpleBranch (l,_) -> l
  | CompoundBranch (l,_) -> l in
  let rec balance = fun m -> match m with
  | (b1,b2) -> if (getl(b1)*getw(b1))=(getl(b2)*getw(b2)) then (match (b1,b2) with
  | (SimpleBranch (_,_), SimpleBranch(_,_)) -> true
  | (SimpleBranch (_,_), CompoundBranch(_,b)) -> true&&(balance b)
  | (CompoundBranch (_,b), SimpleBranch(_,_)) -> true&&(balance b)
  | (CompoundBranch (_,ba), CompoundBranch(_,bb)) -> true&&(balance ba)&&(balance bb))
  else false in balance m;;
