exception Not_implemented
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
	match next with
	|nhd::ntl ->(
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
    |_ -> raise Not_implemented)
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
