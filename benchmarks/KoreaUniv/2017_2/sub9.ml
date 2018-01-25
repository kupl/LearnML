(*problem 1*)
  type btree = Empty | Node of int * btree * btree

  let rec mirror : btree -> btree
  =fun t -> (match t with 
      |Node (n, Empty, Empty) -> Node(n, Empty, Empty)
      |Node (n,x,y) -> Node (n,(mirror(y)),(mirror(x)))
      |Empty -> Empty)



  (*problem 2*)
  type nat = ZERO | SUCC of nat

  let rec natadd : nat -> nat -> nat
  = fun n1 n2 -> (match n1 with 
      |ZERO -> n2
      |SUCC(x) -> SUCC(natadd x n2))

  let rec natmul : nat -> nat -> nat
  = fun n1 n2 -> (match n2 with 
      |ZERO -> ZERO
      |SUCC(x) -> natadd n1 (natmul n1 x))

  let rec natexp : nat -> nat -> nat
  = fun n1 n2 -> (match n2 with 
      |ZERO -> SUCC(ZERO)
      |SUCC(x) -> natmul n1 (natexp n1 x))


(*Probleml 3 *)





  (*Problem 4*)
  
  type aexp = 
  |Const of int
  |Var of string 
  |Power of string * int
  |Times of aexp list
  |Sum of aexp list

  let rec diff : aexp * string -> aexp
  = fun (e,x) -> (match e with 
      |Const k -> Const 0
      |Var v -> if v = x then Const 1 else Const 0
      |Power (v,n) -> if v = x then 
      if n = 0 then Const 0
      else if n =1 then Const 1 
      else  Times [Const n; Power(v, n-1)]
      else Const 0
      |Times l -> (match l with 
        |hd ::[] -> diff (hd,x)
        |hd ::tl -> Sum[Times [diff(hd,x); Times tl]; Times [hd;diff(Times(tl),x)]])
      |Sum l -> match l with 
        |hd::[] -> diff(hd,x)
        |hd::tl -> Sum[diff(hd,x); diff(Sum(tl), x)])

    
(*Problem 5*)
 type exp = X
 |INT  of int 
 |ADD of exp * exp
 |SUB of exp * exp
 |MUL of exp * exp 
 |DIV of exp * exp
 |SIGMA of exp * exp *exp

 let rec sigf f x = 
 match f with 
 |X -> (match x with 
     |INT (n) -> n
     |_ -> raise (Failure "It must be INT"))
 
 |INT n -> n
 |ADD (a,b) -> (sigf a x) + (sigf b x)
 |SUB (a,b) -> (sigf a x) - (sigf b x)
 |MUL (a,b) -> (sigf a x) * (sigf b x)
  |DIV (a,b) -> (sigf a x) / (sigf b x)

  let rec sigma (a,b,c) =
   if (sigf a a) < (sigf b b) then (sigf c a) + sigma(INT((sigf a a)+1),b,c) else (sigf a c)

       let rec calculator: exp -> int
       = fun e -> (match e with 
         |INT (num) -> num 
         |ADD (a,b) -> calculator(a) + calculator (b)
         |SUB (a,b) -> calculator(a) - calculator (b)
         |MUL (a,b) -> calculator(a) * calculator (b) 
         |DIV (a,b) -> calculator(a) / calculator (b) 
         |SIGMA (a,b,c) -> (match c with 
           |SIGMA(x,y,z) -> (calculator(b) - calculator(a)+1)*sigma(x,z,y)
           |_ -> sigma(a,b,c)))





  (* Problem 6*)
  type mobile = branch * branch (* left and right branches *)
  and branch = SimpleBranch of length * weight
              |CompoundBranch of length * mobile 
  and length = int 
  and weight = int
  
  let rec checkw
  = fun m -> match m with 
  |(l,r) -> match l with 
  |SimpleBranch(l1,w1)-> (match r with
      |SimpleBranch(l2, w2) -> w1 + w2
      |CompoundBranch(l,x) -> checkw(x) +w1)
  |CompoundBranch(l,x) -> (match r with 
      |SimpleBranch(l2,w2) -> checkw(x) +w2
      |CompoundBranch(l,y) -> checkw(x) + checkw(y))


  let rec balanced : mobile -> bool
  = fun m -> (match m with 
      |SimpleBranch(l1,w1), SimpleBranch(l2, w2) -> if (l1*w1) = (l2*w2) then true else false
      |CompoundBranch(l1,y), SimpleBranch(l2,w2) -> if balanced y = true && (checkw(y)*l1)= (l2* w2)  then true else false
      |SimpleBranch(l1,w1), CompoundBranch(l2,x) -> if balanced x =true && (l1*w1) = (l2*checkw(x)) then true else false
      |CompoundBranch(l1,x), CompoundBranch(l2,y) -> if balanced x = true && balanced y =true && (l1* checkw(x)) = (l2* checkw(y)) then true else false)


  (* Problem 7*)
  type digit = ZERO |ONE
  type bin = digit list

  let rec place n = if n > 0 then 2 * place(n-1) else 1
  let rec btd = fun n -> match n with 
  |[] -> 0
  |hd:: tl -> if hd = ONE then (place ((List.length n)-1)) + (btd tl) else (btd tl) 
  
  let rec dtb = fun n -> if n >0 then (if (n mod 2) =1 then (dtb (n/2))@[ONE] else (dtb (n/2)) @[ZERO]) 
  else []

  let bmul : bin -> bin -> bin
  =fun b1 b2 -> let x = ((btd b1 ) *( btd b2)) in dtb x
