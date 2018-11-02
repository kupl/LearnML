(*problem 3*)
let rec fastexpt : int -> int -> int
= fun b n ->
if n = 0 then 1 else
(if n mod 2 = 0 
  then fastexpt b (n/2) * fastexpt b (n/2) 
else fastexpt b (n-1) * b);;


type formula =
    True
  | False
  | Var of string
  | Neg of formula
  | And of formula * formula
  | Or of formula * formula
  | Imply of formula * formula
  | Iff of formula * formula

type value3 = Bool of bool
type env3 = (string * value3) list
  let empty_env3 = []
  let extend_env3 (x,v) e = (x,v)::e
  let rec apply_env3 e x =
  match e with
  [] -> raise (Failure ("variable" ^x^"not found"))
  | (y,v)::tl -> if x = y then v else apply_env3 tl x

let rec eval3: formula -> env3 -> value3
= fun formula env3 ->
  match formula with
   True -> Bool true
  |False -> Bool false
  |Var x -> (apply_env3 env3 x)
  |Neg e ->
    if (eval3 e env3)= Bool false then Bool true
    else Bool false
  |And (e1,e2)->
  if (eval3 e1 env3)= Bool true then
    if (eval3 e2 env3) = Bool true then Bool true
    else Bool false
  else Bool false
  |Or (e1,e2) ->
  if (eval3 e1 env3)= Bool false then
    if (eval3 e2 env3) = Bool false then Bool false
    else Bool true
  else Bool true
  |Imply (e1,e2) ->
    if (eval3 e1 env3) = Bool true then
      if (eval3 e2 env3) = Bool false then Bool true
      else Bool false
    else Bool false
  
  |Iff (e1,e2) ->
    if (eval3 e1 env3) = (eval3 e2 env3) then Bool true
    else Bool false

let vs = []
let rec findv formula = 
  match formula with
   Var x -> [x]
  |Neg x-> (findv x)@vs
  |And(x,y)->(findv y)@((findv x)@vs)
  |Or(x,y)->(findv y)@((findv x)@vs)
  |Iff(x,y)->(findv y)@((findv x)@vs)
  |Imply(x,y)-> (findv y)@((findv x)@vs)
  |_ -> []

let rec lstleng lst=
  match lst with
  []->0
  |hd::tl -> 1+(lstleng tl)



let rec binary num=
  match num with
  0 -> []
  |_ -> (binary (num/2))@[(num mod 2)]

let rec signexten lst fulllst=
  if lstleng lst < lstleng fulllst then signexten ([0]@lst) fulllst
  else lst

let rec set numlst lst=   
  match numlst with
  []-> []
  |head::tail->
  if head =0 then
    match lst with
    []-> []
    |hd::tl-> (hd,Bool false)::(set tail tl)
  else
      match lst with
    []-> []
    |hd::tl-> (hd,Bool true)::(set tail tl)


let rec st formula n=
let v1 = fastexpt 2 (lstleng (findv formula)) in
  if n=v1 then Bool false
  else
    if eval3 formula (set (signexten (binary n) (binary (v1-1))) (findv formula)) = Bool true then
    Bool true
    else st formula (n+1)


let sat : formula -> bool
= fun f -> 
let v1= st f 0 in
  match v1 with
  Bool n -> n