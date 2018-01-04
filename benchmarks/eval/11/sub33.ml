type expr = NUM of int
| PLUS of expr * expr
| MINUS of expr * expr
| MULT of expr * expr
| DIVIDE of expr * expr
| MAX of expr list

let rec eval expr=(match expr with
        NUM(x)->x
        |PLUS(e1,e2)->eval(e1)+eval(e2)
        |MINUS(e1,e2)->eval(e1)-eval(e2)
        |MULT(e1,e2)->eval(e1)*eval(e2)
        |DIVIDE(e1,e2)->eval(e1)/eval(e2)
        |MAX(l)->if l=[] then 0
                else if List.tl(l)=[] then eval(List.hd(l))
                else if eval(List.hd(l))>eval(List.hd(List.tl(l))) then
                        eval(MAX(List.hd(l)::List.tl(List.tl(l))))
                else eval(MAX(List.tl(l)))
        )

(*
eval(PLUS(NUM 4,NUM 5))
eval(MINUS(NUM 4,NUM 5))
eval(MULT(NUM 4,NUM 5))
eval(DIVIDE(NUM 4,NUM 5))
eval(MAX([]))
eval(MAX([NUM 3;NUM 4;NUM 2;NUM 23;NUM 241;NUM 2]))
 *)
