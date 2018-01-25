(* problem 1*)
type btree = Empty | Node of int * btree * btree

let rec mirror : btree -> btree
= fun t -> 
  match t with
  | Empty -> Empty
  | Node(p,r,l) -> Node(p, (mirror l), (mirror r))


(* problem 2*)
type nat = ZERO | SUCC of nat

let rec natadd : nat -> nat -> nat 
= fun n1 n2 -> 
  if n1 = ZERO then n2
  else match n2 with 
        | ZERO -> n1
        | SUCC a -> natadd (SUCC n1) a

let rec natmul : nat -> nat -> nat 
= fun n1 n2 -> (* TODO *)
  if n1 = ZERO || n2 = ZERO then ZERO
else if n1 = SUCC ZERO then n2
else if n2 = SUCC ZERO then n1
else match n1 with 
	| ZERO -> ZERO
	| SUCC a -> natadd (natmul a n2) n2

let rec natexp : nat -> nat -> nat 
= fun n1 n2 -> (* TODO *)
  match n2 with
  | ZERO -> SUCC ZERO
  | SUCC a -> if a = ZERO then n1
              else natmul n1 (natexp n1 a)

(* problem 3*)
type formula =
    True
  | False
  | Var of string
  | Neg of formula
  | And of formula * formula
  | Or of formula * formula
  | Imply of formula * formula
  | Iff of formula * formula

let rec scan f varlist = 
  match f with
  | Var x -> (
              match varlist with 
              | [] -> x::[]
              | hd::tl-> if hd=x then varlist else (scan f tl)@varlist
              )
  | Neg a -> scan a varlist
  | And (a1,a2) -> scan a2 (scan a1 varlist)
  | Or (a1,a2) -> scan a2 (scan a1 varlist)
  | Imply (a1,a2) -> scan a2 (scan a1 varlist)
  | Iff (a1,a2) -> scan a2 (scan a1 varlist)
  | _-> varlist

let rec finList hd posslist = 
  match posslist with 
  |[]->[]
  |ph::pt -> ((hd,true)::ph)::((hd,false)::ph)::(finList hd pt)

let rec makeEnv varlist posslist= 
  match varlist with 
  | [] -> posslist
  | hd::tl -> (
              match posslist with
              |[]->(makeEnv tl [[(hd,true)];[(hd,false)]])
              |ph::pt-> makeEnv tl (finList hd posslist)
              )


let rec apply_env x env =
  match env with
  | [] -> false
  | (y,v)::tl -> if x = y then v else (apply_env x tl)

let rec resolve f givenEnv =
  match f with
  | True -> true
  | False -> false
  | Var x -> apply_env x givenEnv
  | Neg a -> if (resolve a givenEnv) = true then false else true
  | And (a1,a2) ->(let b1 = (resolve a1 givenEnv) in let b2 = (resolve a2 givenEnv) in (b1&&b2))
  | Or (a1,a2) ->(let b1 = (resolve a1 givenEnv) in let b2 = (resolve a2 givenEnv) in (b1||b2))
  | Imply (a1,a2) -> (let b1 = (resolve a1 givenEnv) in let b2 = (resolve a2 givenEnv) in (if (b1=true)&&(b2=false) then false else true)) 
  | Iff (a1,a2) -> (let b1 = (resolve a1 givenEnv) in let b2 = (resolve a2 givenEnv) in (b1=b2))


let rec untiltrue f envlist =
  match envlist with
  | []->resolve f []
  | hd::tl -> if (resolve f hd)=true then true else (untiltrue f tl)


let rec sat : formula -> bool
= fun f -> (* TODO *)
    let vars = scan f [] in
      let envlist = makeEnv vars [] in
          untiltrue f envlist

(* problem 4*)
type aexp =
  | Const of int
  | Var of string
  | Power of string * int
  | Times of aexp list
  | Sum of aexp list

let rec diff : aexp * string -> aexp
= fun (e,x) -> (* TODO *)
  match e with
	|Const c -> Const 0
	|Var y -> if y=x then Const 1 else Const 0
	|Power (z,a) -> (if z=x then Times [Const a; Power (z,a-1)] else Const 0)
  |Sum sl ->( 
        match sl with
        | [] -> Sum []
        | hd::tl -> 
        (
          let newt = diff (Sum tl, x) in 
            (match newt with 
              |Sum ntl -> Sum (diff (hd,x)::ntl)
              |_-> Sum [diff (hd,x)]
            )
        ))
  |Times l ->(
        match l with 
        |[]-> Const 0
        |hd::tl -> (Sum [(Times ([diff (hd,x)]@tl)); (Times [hd;diff (Times tl,x)])])
        )

(* problem 5*)

type exp = X
         | INT of int
         | ADD of exp * exp
         | SUB of exp * exp
         | MUL of exp * exp
         | DIV of exp * exp
         | SIGMA of exp * exp * exp


let rec calculator : exp -> int
= fun e -> (* TODO *)
	match e with 
	| X -> raise (Failure "Error")
	| INT n -> n
	| ADD(a,b) -> ((calculator a) + (calculator b))
	| SUB(a,b) -> ((calculator a) - (calculator b))
	| MUL(a,b) -> ((calculator a) * (calculator b))
	| DIV(a,b) -> let vb = calculator b in 
				(if vb = 0 then raise (Failure "Error") 
				else ((calculator a)/vb))
	| SIGMA (e1,e2,vv) -> 
		let v1 = calculator e1 in
		let v2 = calculator e2 in
		(if v1>v2 then 0 else ((calc v1 vv) + (calculator (SIGMA (INT (v1+1), INT v2,vv) ))))

and calc ie1 vv = 
	match vv with
	| X -> ie1
	| INT n -> n
	| ADD(a,b) -> ((calc ie1 a) + (calc ie1 b))
	| SUB(a,b) -> ((calc ie1 a) - (calc ie1 b))
	| MUL(a,b) -> ((calc ie1 a) * (calc ie1 b))
	| DIV(a,b) -> let vb = calc ie1 b in 
				(if vb = 0 then raise (Failure "Error") 
				else ((calc ie1 a)/vb))
	| SIGMA(a1,a2,av) -> calculator (SIGMA(a1,a2,av))

	
(* problem 6*)
type mobile = branch * branch     (* left and rigth branches *)
and branch = SimpleBranch of length * weight
           | CompoundBranch of length * mobile
and length = int
and weight = int
 

let rec getw m =
	match m with (b1,b2) -> ((match b1 with 
									| SimpleBranch (l,w) -> w
									| CompoundBranch (l,sm) -> getw sm)
								+(match b2 with 
									| SimpleBranch (l,w) -> w
									| CompoundBranch (l,sm) -> getw sm))

let rec torq a = 
    match a with 
    | SimpleBranch (l,w) -> l*w
    | CompoundBranch (l,m) -> (match m with (lb, rb) -> l*(getw m))

let rec balanced : mobile -> bool
= fun m -> (* TODO *)
    match m with
    | (b1, b2) -> if torq b1 = torq b2 then 
    								((match b1 with 
									| SimpleBranch (l,w) -> true
									| CompoundBranch (l,sm) -> (balanced sm)) 
    								&&
    								(match b2 with 
									| SimpleBranch (l,w) -> true
									| CompoundBranch (l,sm) -> (balanced sm)))
                else false


(* problem 7*)
type digit = ZERO | ONE
type bin = digit list

let rec bd a = 
    match a with
    | []->[]
    | hd::tl -> (bd tl)@[hd]

let rec dadd a1 a2 =
    match a1 with
    | []-> a2
    | h1::t1 -> 
        (match a2 with 
        | []-> a1
        | h2::t2 -> if h1=h2 then 
                        (if h1=ONE then ZERO::(dadd (dadd [ONE] t1) t2)
                        else (ZERO::dadd t1 t2))
                    else ONE::(dadd t1 t2))

let rec dmul a b =
    match b with
    | [] -> []
    | hd::tl -> if hd = ONE then dadd a (ZERO::(dmul a tl))
                else (ZERO::(dmul a tl))

let bmul : bin -> bin -> bin
= fun b1 b2 -> (* TODO *)
    bd (dmul (bd b1) (bd b2))
