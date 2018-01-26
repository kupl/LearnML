exception NotImplemented

(*********************)
(*     Problem 1     *)
(*********************)
module Problem1 = struct
  type aexp = 
  | Const of int
  | Var of string
  | Power of string * int
  | Times of aexp list
  | Sum of aexp list

  let rec diff : aexp * string -> aexp
  = fun (exp, var) ->
    match exp with 
    | Const a -> Const 0
    | Var a -> if (a=var) then Const 1 else Const 0
    | Power(a, b) -> if (a=var) then Times[Const b; Power(a, b-1)] else Const 0;
    | Times [] -> Const 0
    | Times(hd::tl) -> Sum[Times(diff(hd, var)::tl); Times[hd; diff(Times tl, var)]]
    | Sum [] -> Const 0 
    | Sum(hd::[]) -> diff(hd, var)
    | Sum(hd::tl) ->  Sum[diff(hd, var); diff(Sum tl, var)];;
end

(*********************)
(*     Problem 2     *)
(*********************)
module Problem2 = struct
type mobile = branch * branch
  and branch = SimpleBranch of length * weight
             | CompoundBranch of length * mobile
  and length = int
  and weight = int
  
let rec getWeight
=fun x -> match x with
    | CompoundBranch(length,(lb,rb))->(getWeight lb)+(getWeight rb)
    | SimpleBranch(length,weight)->weight;;

let resultForBalance
=fun x -> match x with
    | CompoundBranch (length, (lb,rb)) -> length * ((getWeight lb) + (getWeight rb))
    | SimpleBranch (length, weight) -> length * weight;;

let rec balanced : mobile -> bool
=fun (lb,rb) ->match lb with
  | CompoundBranch (l1, (lb1, rb1)) ->  if(balanced (lb1, rb1) = false) then false
    else (match rb with
    | SimpleBranch (l2,w2) -> if ((resultForBalance lb) = (resultForBalance rb)) then true
    else false
    | CompoundBranch (l2, (lb2, rb2)) -> if (balanced (lb2, rb2) = false) then false
    else if ((resultForBalance lb) = (resultForBalance rb)) then true else false)
  | SimpleBranch (l1, w1) -> (match rb with
    | SimpleBranch (l2, w2) -> if ((resultForBalance lb) = (resultForBalance rb)) then true
    else false
   | CompoundBranch (l2,(lb1,rb1)) -> if((balanced (lb1, rb1) = true) && ((resultForBalance lb) = (resultForBalance rb))) then true else false);;                                                               
end

(*********************)
(*     Problem 3     *)
(*********************)
module Problem3 = struct
  type exp =
  | X
  | INT of int
  | ADD of exp * exp
  | SUB of exp * exp
  | MUL of exp * exp
  | DIV of exp * exp
  | SIGMA of exp * exp * exp

  let rec calculator : exp -> int
  = fun exp -> match exp with
    | X -> raise(Failure "NotImplemented, Wrong input")
    | INT a -> a
    | ADD (f1, f2) -> calculator f1 + calculator f2
    | SUB (f1, f2) -> calculator f1 - calculator f2
    | MUL (f1, f2) -> calculator f1 * calculator f2
    | DIV (f1, f2) -> calculator f1 / calculator f2
    | SIGMA (f1, f2, f3) -> 
      let in1 = calculator f1 in
        let in2 = calculator f2 in
          if in1=in2 then
            begin
                  let rec calsig = fun x y ->
                    match x with
                    | X -> calsig y y
                    | INT n -> n
                    | ADD (a, b)->(calsig a y)+(calsig b y)
                    | SUB (a, b)->(calsig a y)-(calsig b y)
                    | MUL (a, b)->(calsig a y)*(calsig b y)
                    | DIV (a, b)->(calsig a y)/(calsig b y)
                    | SIGMA (a, b, c) -> calculator x in calsig f3 f2
            end
          else
            calculator (SIGMA (f2, f2, f3))+calculator (SIGMA (f1, SUB(f2, INT 1), f3));;
        
      (*let rec calsig = fun a b f 
        -> if (calculator a=calculator b) then f(calculator a) 
            else f(calculator a)+calsig (calculator a+1) (calculator b) f
              in calsig (calculator f1) (calculator f2) f3;;*)
end

(*********************)
(*     Problem 4     *)
(*********************)
module Problem4 = struct 
  type exp = V of var
           | P of var * exp
           | C of exp * exp
  and var = string

  let check : exp -> bool
  =fun e -> true;;

  let rec isFree : var * string list -> bool 
  =fun (v, l) -> match l with
    | [] -> false
    | hd :: tl -> if (v = hd) then true else isFree (v,tl);;

  let rec checkForm : exp * string list -> bool
  = fun (e, l) -> match e with
    | V ((a:var)) -> isFree (a, l)
    | P ((v:var), e1) -> checkForm (e1, (l @ [v]))
    | C (e1, e2) -> if ((checkForm(e1, l) && checkForm(e2, l)) = true) then true else false;;

  let check  : exp -> bool
  =fun e -> checkForm (e, []);;

end