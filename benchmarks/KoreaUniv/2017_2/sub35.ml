(*problem 1*)

type btree=Empty |Node of int *btree *btree

let rec mirror : btree -> btree
=fun t->
     
      match t with
      |Empty -> Empty
      |Node (a, left, right) -> if a > 0 then Node(a, mirror right, mirror left) else raise(Failure("Number must be nat"))
     (* |Node(_,_,_)-> raise (Failure("Number must be nat"))*)
  ;;


(*problem 2*)

  type nat = ZERO | SUCC of nat

  let rec natadd : nat -> nat -> nat
  =fun n1 n2 -> 
        match n1 with 
        |ZERO->n2
        |SUCC(n1)->natadd n1 (SUCC(n2))
  
  let rec natmul : nat -> nat -> nat
  =fun n1 n2 ->
        match n1 with
        |ZERO -> ZERO
        |SUCC(n1) -> natadd n2 (natmul n1 n2)

  let rec natexp : nat -> nat -> nat
  =fun n1 n2 ->
        match n1 with
        |ZERO -> n2
        |SUCC(n1) -> natmul n2 (natexp n1 n2)

  ;;

(*problem 3*)

  type formula =
      True
      |False
      |Var of string
      |Neg of formula
      |And of formula *formula
      |Or of formula *formula
      |Imply of formula *formula
      |Iff of formula *formula

  let sat : formula -> bool
  =fun f ->
        true           
            
  
  ;;


(*probem 4*)(* [] cannot find how to put in [] 
  type aexp=
  | Const of int
  | Var of string
  | Power of string *int
  | Times of aexp list
  | Sum of aexp list

  let rec diff : aexp*string -> aexp
  =fun (e,x) -> 
      match e with
      
      |Const a -> Const 0
      |Var b -> if b = x then Const 1 else Var x 
      |Power (s,i) -> if  s= x then Times [ Const i; Power(s,i-1)] else Power (s,i)
      |Times a,_ -> raise(Failure("nothing to differentiate"))
      |Times l -> if l = [] then Const 0 else 
                  begin 
                  match l with 
                  |h::t-> Sum [Times[diff(h,"x")];diff(h,"x")]
                  end
      |Sum n, []-> raise(Failure("nothing to differentiate"))
      |Sum m -> if m=[] then Const 0 else
                match m with 
                |h::t -> Sum [diff(h,"x"); diff(h,"x")]
               ;; 
*)
(*problem 5*) 
  type exp = X
    |INT of int
    |ADD of exp *exp
    |SUB of exp *exp
    |MUL of exp *exp
    |DIV of exp *exp
    |SIGMA of exp *exp *exp

    
       let rec change:exp *int ->exp
       =fun(e,k) ->
          match e with 
          |X -> INT k
          |INT n -> INT n
          |ADD(a,b) -> ADD(change(a,k),change(b,k))
          |SUB(a,b) -> SUB(change(a,k),change(b,k))
          |MUL(a,b) -> MUL(change(a,k),change(b,k))
          |DIV(a,b) -> DIV(change(a,k),change(b,k))
          |SIGMA(a,b,c) -> change(c,k)

     let find: exp->int 
     = fun e->
        let rec calculate: exp-> int
        =fun e->
          match e with
          |X->1
          |INT n->n
          |ADD(a,b) -> calculate(a)+calculate(b)
          |SUB(a,b) -> calculate(a)-calculate(b)
          |MUL(a,b) -> calculate(a)*calculate(b)
          |DIV(a,b) -> calculate(a)/calculate(b)
          |SIGMA(a,b,c)-> if calculate(a)>calculate(b) then 0 
   else calculate(change(SIGMA(a,b,c),calculate(a)))+calculate(SIGMA(ADD(a,INT 1),b,c))
    in calculate(e)
                          
;;

(*problem 6*)

  type mobile =branch * branch
  and branch=SimpleBranch of length *weight
            |CompoundBranch of length * mobile
  and length = int
  and weight = int

    let rec wei:branch ->int
    =fun n ->
      match n with
      |SimpleBranch(a,b)->a*b
      |CompoundBranch(a,(x,y))->a*(wei(x)+wei(y))
  let balanced : mobile -> bool
  =fun m->
    let rec balance :mobile->bool
  = fun m->
    match m with
    |(SimpleBranch (a,b),SimpleBranch(c,d))->
    if a*b=c*d then true else false
    |(SimpleBranch (a,b),CompoundBranch(c,(x,y)))->
    if a*b=c*(wei(x)+wei(y)) then true && balance(x,y) else false
    |(CompoundBranch (a,(x,y)),SimpleBranch(c,d))->
    if a*(wei(x)+wei(y))=c*d then true && balance(x,y) else false
    |(CompoundBranch(a,(x,y)),CompoundBranch(b,(z,w)))->
    if a*(wei(x)+wei(y))=b*(wei(z)+wei(w)) then balance (x,y) && balance(w,z) else false
  in balance(m)
 (* 
  let rec wei: branch ->int
  =fun n ->
  match n with
  |SimpleBranch(a,b)->a*b
  |CompoundBranch(a,(x,y))-> a*(wei(x)+wei(y))
;;
*)
(*problem 7*)

  type digit = ZERO | ONE
  type bin = digit list
      
  let rec decimal: bin-> int 
  =fun b->
      let num=0 in
      match b with
      |[]-> num
      |h::t-> 
(*
            match h with
            |ZERO-> decimal t
            |ONE-> 
*)
          if h =ONE 
          then let num = num + int_of_float (2.0**float_of_int (List.length t)) 
               in num
          else decimal t 
        
  let rec findbinary: int-> bin-> bin
  =fun x lst ->
    match x with
    |0->[ZERO]@lst
    |1->[ONE]@lst
    |_-> findbinary (x/2) (if x mod 2 = 0 then [ZERO]@lst else [ONE]@ lst)
    (*
findbinary (x/2) (lst::(if y mod 2 = 0 then ZERO else ONE))
  *)

  let bmul: bin->bin->bin
  =fun b1 b2->
      let a= decimal(b1)*decimal(b2) in
      findbinary a []  

  (*
      let rec findbinary: int->bin
      =fun x lst ->
        match x with 
        0-> lst
        |_-> findbinary (x/2) (lst::(if y mod 2=0 then ZERO else ONE))
      in findbinary a []
  *) 
      ;;




      
      
