exception Notree
exception Leafdraw


type team = Korea | France | Usa | Brazil | Japan | Nigeria | Cameroon | Poland
| Portugal | Italy | Germany | Sweden | England | Croatia | Argentina  

type tourna = LEAF of team
| NODE of tourna * tourna

type branch = tourna * int * int


let getmax (a,b) =
    if a > b then a else b

let rec getdepth (t:tourna) =
    match t with
    | LEAF a -> 0
    | NODE (a, b) -> 1 + getmax(getdepth (a), getdepth (b))

let rec getbranchlist(t:tourna list) =
    match t with
    | [] -> []
    | hd::td -> (match hd with
    | LEAF a -> (LEAF a, 0, 0) :: getbranchlist(td)
    | NODE (a, b) -> (NODE (a, b), 0, getdepth(NODE(a,b))) :: getbranchlist(td)
    )

let rec endtest(t:branch list) =
    match t with
    | [] -> true
    | hd :: td -> (match hd with
    | (LEAF _, _, _) -> true && endtest(td)
    | (NODE _, _, _) -> false
    )

let rec childbranch(t:branch list) =
    match t with
    | [] -> []
    | hd::td -> (match hd with
    | (LEAF a, b, c) -> (LEAF a, b+1, c) :: childbranch(td)
    | (NODE (a, b), c, d) -> (a, 0, d-1) :: (b, 0, d-1) :: childbranch(td)
    )

let rec printchar ((a:string), (t:int)) =
    if (t = 1) then Printf.printf "%s" a
    else if (t > 1) then((Printf.printf "%s" a); 
    printchar (a, t-1) )


let rec power (a,n) =
if n = 0 then 1 else
a * power(a,n-1)

let rec draw (a:branch list) =
    match a with
    | [] -> printchar("\n",1);
    | hd::td -> (match hd with
    | (LEAF a, b, c) -> 
        (printchar(" ", power(2,c)-1));
        (printchar("|",1));
        (printchar(" ",power(2,c)));
        draw(td)

    | (NODE (a, b), c, d) -> (printchar(" ",power(2, d-1)-1));
    (printchar("|",1));
    (printchar("-",power(2,d) - 1));
    (printchar("|",1));
    (printchar(" ",power(2, d-1)));
    draw(td)
    )

let rec drawwhole (a:branch list) =
    if endtest(childbranch(a)) then
        draw(a)
    else ((draw(a));
    drawwhole(childbranch(a)))

let pptree (t:tourna) =
    match t with
    | LEAF _ -> raise Notree
    | NODE (a, b) -> (printchar(" ",power(2, (getdepth(t)))-1 ));
    (printchar("|\n",1));
    drawwhole(getbranchlist([t]))



