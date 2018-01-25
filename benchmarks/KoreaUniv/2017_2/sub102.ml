exception Not_implemented

(* problem 1*)
type btree = Empty | Node of int * btree * btree

let rec mirror : btree -> btree
= fun t ->
  match t with
  |Empty -> Empty
  |Node(num,b1,b2) ->
    Node(num,mirror(b2),mirror(b1))
    
(* problem 2*)
type nat = ZERO | SUCC of nat

let rec natadd : nat -> nat -> nat 
= fun n1 n2 ->
  match n1 with
  | ZERO -> n2
  | SUCC(temp) ->
    SUCC(natadd temp n2)

let rec natmul1 : nat -> nat -> nat -> nat
= fun n1 n2 n22 ->
  match n1 with
  |ZERO -> ZERO
  |SUCC(newn1) ->
    (match n2 with
     |ZERO -> natmul1 newn1 n22 n22
     |SUCC(newn2)->
      SUCC(natmul1 n1 newn2 n22)
    )

let rec natmul : nat -> nat -> nat 
= fun n1 n2 ->
  natmul1 n1 n2 n2

let rec natexp1 : nat-> nat -> nat -> nat
= fun n1 n11 n2 ->
  match n2 with
  |ZERO -> n1
  |SUCC(newn2) ->
    let newn1 = natmul n1 n11 in
    natexp1 newn1 n11 newn2

let rec natexp : nat -> nat -> nat 
= fun n1 n2 ->
  match n2 with
  |ZERO -> (SUCC ZERO)
  |SUCC(newn2) -> natexp1 n1 n1 newn2

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


let rec putvar :('a * 'b)list list -> string -> ('a * 'b)list list
=fun vl s ->
  match vl with
  |[] -> []
  |hd::tl ->
    let a1 = hd@[(s,true)] in (*[(s,tf)]*)
    let a2 = hd@[(s,false)] in (*[(s,tf)]*)
    a1::a2::(putvar tl s)(*[[(s1,tf1)];[(s2,tf2)]]*)

let rec check0 : ('a * 'b)list -> string -> bool
= fun vl s ->
  match vl with
  |[] -> true
  |hd::tl ->
    let (s1,tf1) = hd in
    if s1 = s then false
    else check0 tl s

let check :('a * 'b)list list -> string ->('a * 'b)list list
= fun vl s ->
  match vl with
  |[] -> [[(s,true)];[(s,false)]]
  |hd::tl ->
    let tf = check0 hd s in
    (match tf with
      |true -> putvar vl s
      |false -> vl
    )
    
let rec setvarlist : ('a * 'b)list list -> formula ->('a * 'b) list  list
= fun vl f ->
  match f with
  |True -> []
  |False ->[]
  |Var s ->
    check vl s 
  |Neg f1 -> 
    let v1 = setvarlist vl f1 in
    v1
  |And (f1,f2) -> 
    let v1 = setvarlist vl f1 in
    let v2 = setvarlist v1 f2 in
    v2
  |Or (f1,f2) ->
    let v1 = setvarlist vl f1 in
    let v2 = setvarlist v1 f2 in
    v2
  |Imply (f1,f2)->
    let v1 = setvarlist vl f1 in
    let v2 = setvarlist v1 f2 in
    v2
  |Iff (f1,f2) ->
    let v1 = setvarlist vl f1 in
    let v2 = setvarlist v1 f2 in
    v2

let rec helprun : ('a * 'b) list -> string -> bool
= fun l s ->
  match l with
  |[] -> false
  | hd::tl ->
    let (s1,tf1) = hd in
    if s1 = s then tf1
    else helprun tl s
    
let rec run : ('a * 'b) list -> formula -> bool
= fun l f ->
  match f with
  |True -> true
  |False -> false
  |Neg f1 ->
    let ret1 = run l f1 in
    if ret1 = true then false
    else true
  |Var s ->
    helprun l s
  |And (f1,f2) ->
    let ret1 = run l f1 in
    let ret2 = run l f2 in
    (match (ret1,ret2) with
      |(true,true) -> true
      |(_,_) -> false
    )
  |Or (f1,f2) ->
    let ret1 = run l f1 in
    let ret2 = run l f2 in
    (match (ret1,ret2) with
     |(false,false) -> false
     | (_,_) -> true
    )
  |Imply (f1,f2) ->
    let ret1 = run l f1 in
    let ret2 = run l f2 in
    (match (ret1,ret2) with
     |(true,false) -> false
     |(_,_) -> true
    )
  |Iff (f1,f2) ->
    let ret1 = run l f1 in
    let ret2 = run l f2 in
    if ret1 = ret2 then true
    else false

let rec realrun : ('a * 'b)list list -> formula -> bool
= fun vl f ->
  match vl with
  |[] -> false
  |hd::tl -> 
   let a1 = run hd f in
   let a2 = realrun tl f in
   a1||a2

let sat : formula -> bool
= fun f ->
  let varlist = setvarlist [] f in
  match varlist with
  |[] -> run [] f
  |_ -> realrun varlist f
  

(*problem 4*)
type aexp =
  | Const of int
  | Var of string
  | Power of string * int
  | Times of aexp list
  | Sum of aexp list
  
let easy : aexp*string ->aexp
= fun (e,x) ->
  match e with
  |Const y -> Const 0
  |Var y -> 
    if y = x then Const 1
    else Const 0
  |Power(y,i) ->
    if y=x then begin
     if i = 2 then Times([Const i;Var x])
     else begin
      if i = 1 then Const 1
      else Times([Const i; Power(x,(i-1))])
     end
    end
    else Const 0
  |_ -> raise Not_implemented

let rec hard : aexp*string -> aexp list -> aexp list -> aexp list
= fun (e,x) prev next->
  let nhd::ntl = next in
  match e with
   |Times al1 ->
    (match ntl with
      |[] -> 
        (match nhd with
         |Sum al2 ->
          let diffnow = hard(Sum al2,x) [] al2 in
          let now = (prev@diffnow) in
          (match prev with
            |[] -> [Sum[Times(now)]]
            |_ -> [Times(now)]
          )
         |Times al2 ->
          let diffnow = hard(Times al2,x) [] al2 in
          let now = (prev@diffnow) in
          (match prev with
            |[] -> [Sum[Times(now)]]
            |_ -> [Times(now)]
          )
         |_ ->
          let diffnow = easy (nhd,x) in
          let now = (prev@[diffnow]) in
          (match prev with
            |[] -> [Sum[Times(now)]]
            |_ -> [Times(now)]
          )
        )
      |_ ->
        (match nhd with (*hd : exp tl:exp list*)
         |Sum al2 ->
          let diffnow = hard(Sum al2,x) [] al2 in
          let now = (prev@diffnow)@ntl in
          let later = hard (Times al2,x) (prev@[nhd]) ntl in
          (match prev with
           |[] -> [Sum([Times(now)]@later)]
           |_ -> [Times(now)]@later
          )
         |Times al2 ->
          let diffnow = hard (Times al2,x) [] al2 in
          let now = (prev@diffnow)@ntl in
          let later = hard (Times al2,x) (prev@[nhd]) ntl in
          (match prev with
           |[] -> [Sum([Times(now)]@later)]
           |_ ->  [Times(now)]@later
          )
         |_ ->
          let diffnow = easy (nhd,x) in
          let now = (prev@[diffnow])@ntl in
          let later = hard (Times al1,x) (prev@[nhd]) ntl in
          (match prev with
            |[] -> [Sum([Times(now)]@later)]
            |_ -> [Times(now)]@later
          )
        )
      )
    |Sum al1 ->
      (match ntl with
       |[] ->
        (match nhd with
         |Sum al2 ->
          let diffnow = hard(Sum al2,x) [] al2 in
          (match prev with
            |[] ->[Sum(diffnow)]
            |_ -> diffnow
          )
         |Times al2 ->
          let diffnow = hard(Times al2,x) [] al2 in
          (match prev with
            |[] ->[Sum(diffnow)]
            |_ -> diffnow
          )
         |_ ->
          let diffnow = easy(nhd,x) in
          (match prev with
            |[] ->[Sum([diffnow])]
            |_ -> [diffnow]
          )
        )
       |_ ->
        (match nhd with
         |Sum al2 ->
          let diffnow = hard(Sum al2,x) [] al2 in
          let later = hard(Sum al2,x) (prev@[nhd]) ntl in
          (match prev with
           |[] ->[Sum(diffnow@later)]
           |_ -> diffnow@later
          )
         |Times al2 ->
          let diffnow = hard(Times al2,x) [] al2 in
          let later = hard(Sum al2,x) (prev@[nhd]) ntl in
          (match prev with
            |[] ->[Sum(diffnow@later)]
            |_ -> diffnow@later
          )
         |_ ->
          let diffnow = easy (nhd,x) in
          let later = hard(Sum al1,x) (prev@[nhd]) ntl in
          (match prev with
            |[] ->[Sum([diffnow]@later)]
            |_ -> [diffnow]@later
          )
        )
      )
    |_ -> raise Not_implemented
    

let  diff : aexp * string -> aexp
= fun (e,x) ->
  match e with
    |Const y -> easy (e,x)
    |Var y -> easy (e,x)
    |Power (y,i) -> easy (e,x)
    |Times al -> 
      let asdf = hard(Times al,x) [] al in
      List.hd asdf
    |Sum al ->
      let asdf = hard(Sum al,x) [] al in
      List.hd asdf
(* problem 5*)

type exp = X
         | INT of int
         | ADD of exp * exp
         | SUB of exp * exp
         | MUL of exp * exp
         | DIV of exp * exp
         | SIGMA of exp * exp * exp


let result = ref 0         
let rec helpcalculator : exp -> int -> int
=fun e i ->
  match e with
  |INT a -> a
  |SUB (e1,e2)->
    let a1 = helpcalculator e1 i in
    let a2 = helpcalculator e2 i in
    a1-a2
  |MUL (e1,e2)-> 
    let a1 = helpcalculator e1 i in
    let a2 = helpcalculator e2 i in
    a1*a2
  |ADD (e1,e2)->
    let a1 = helpcalculator e1 i in
    let a2 = helpcalculator e2 i in
    a1+a2
  |DIV (e1,e2) ->
    let a1 = helpcalculator e1 i in
    let a2 = helpcalculator e2 i in
    a1/a2
  |X -> i
  |SIGMA (e1,e2,e3)->
    let a1 = (helpcalculator e1 i) in
    let a2 = (helpcalculator e2 i) in
    (match a2-a1 with
      |0 -> helpcalculator e3 a1
      |_ -> let qq = helpcalculator (SIGMA(INT (a1+1),INT a2,e3)) i in
            let rr = helpcalculator e3 a1 in
            qq+rr
    )

let rec calculator : exp -> int
= fun e -> 
  helpcalculator e 0
  

(* problem 6*)
type mobile = branch * branch     (* left and rigth branches *)
and branch = SimpleBranch of length * weight
           | CompoundBranch of length * mobile
and length = int
and weight = int

let trueorflase = ref true

let rec helpbalanced : mobile -> length*weight*length*weight*bool
= fun m ->
  let (lb,rb) = m in
  match (lb,rb) with
  |(SimpleBranch (ll,lw),SimpleBranch (rl,rw)) ->
    if ll*lw = rl*rw then (ll,lw,rl,rw,true)
    else  (ll,lw,rl,rw,false)
  |(SimpleBranch (ll,lw),CompoundBranch (rl,newm)) ->
    let (sonll,sonlw,sonrl,sonrw,torf) = helpbalanced newm in
    if ll*lw = rl*(sonlw+sonrw) then (ll,lw,rl,(sonlw+sonrw),torf)
    else (ll,lw,rl,(sonlw+sonrw),false)
  |(CompoundBranch (ll,newm),SimpleBranch (rl,rw)) ->
    let (sonll,sonlw,sonrl,sonrw,torf) = helpbalanced newm in
    if ll*(sonlw+sonrw) = rl*rw then (ll,(sonlw+sonrw),rl,rw,torf)
    else (ll,(sonlw+sonrw),rl,rw,false)
  |(CompoundBranch (ll,newm1),CompoundBranch (rl,newm2)) ->
    let (sonll1,sonlw1,sonrl1,sonrw1,torf1) = helpbalanced newm1 in
    let (sonll2,sonlw2,sonrl2,sonrw2,torf2) = helpbalanced newm2 in
    let torf = torf1 && torf2 in
    if ll*(sonlw1+sonrw1) = rl*(sonlw2+sonrw2) then (ll,(sonlw1+sonrw1),rl,(sonlw2+sonrw2),torf)
    else (ll,(sonlw1+sonrw1),rl,(sonlw2+sonrw2),false)
  
let balanced : mobile -> bool
= fun m -> 
  let (a,b,c,d,ret) = helpbalanced m in
  ret
  
  
    

(* problem 7*)
type digit = ZERO | ONE
type bin = digit list

let rec getpowertwo : int -> int
= fun i ->
  match i with
  |1 -> 1
  |_ -> 2*getpowertwo (i-1)

let rec getinteger : bin -> int ->int
= fun bl two->
  match bl with
  | [] -> 0
  | hd::tl -> 
    (match hd with
     |ZERO -> getinteger tl (two/2)
     |ONE -> two + getinteger tl (two/2)
    )

let rec getbin : int -> bin
=fun i ->
  match i with
  |0 -> [ZERO]
  |1 -> [ONE]
  |_ ->
    (match (i mod 2) with
      |0 -> [ZERO]@(getbin (i/2))
      |1 -> [ONE]@(getbin ((i-1)/2))
      |_ -> raise Not_implemented
    )

let bmul : bin -> bin -> bin
= fun b1 b2 -> (* TODO *)
  let len1 = List.length b1 in
  let len2 = List.length b2 in
  let two1 = getpowertwo len1 in
  let two2 = getpowertwo len2 in
  let realnum1 = getinteger b1 two1 in
  let realnum2 = getinteger b2 two2 in
  let beforeanswer= realnum1*realnum2 in
  let beforebin = getbin beforeanswer in
  List.rev beforebin

