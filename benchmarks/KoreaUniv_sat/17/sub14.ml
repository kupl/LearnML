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