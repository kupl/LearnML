type team = Korea | France | Usa | Brazil | Japan | Nigeria | Cameroon
| Poland | Portugal | Italy | Germany | Sweden | England
| Croatia | Argentina
type tourna = LEAF of team
| NODE of tourna * tourna

let rec list_make(c,len)=if len>0 then c::list_make(c,len-1) else []
let rec list_mdf(l,n,x)=
        if n>(List.length(l)-1) then list_mdf(l@(list_make(' ',(n-List.length(l)+1))),n,x)
        else if n>0 then List.hd(l)::list_mdf(List.tl(l),n-1,x)
        else x::List.tl(l)
let rec list_print l=if l!=[] then (print_char(List.hd(l));list_print(List.tl
l)) else ()

let rec array_get(a,(x,y))=if y>0 then array_get(List.tl(a),(x,y-1))
        else if x>0 then array_get(List.tl(List.hd(a))::List.tl(a),(x-1,y))
        else List.hd(List.hd(a))
let rec array_make(x,y)=if y>0 then
        list_make(' ',x)::array_make(x,y-1) else []
let rec array_mdf(a,(x,y),n)=
        if y!=0 then List.hd(a)::array_mdf(List.tl(a),(x,y-1),n)
        else list_mdf(List.hd(a),x,n)::List.tl(a)
let rec array_print a=
        if a!=[] then
                (list_print(List.hd(a));print_newline();array_print(List.tl(a)))
        else ()

let rec tree_height t=(match t with 
        NODE(t1,t2)->if (tree_height t1)>(tree_height t2) then (1+tree_height t1)
        else (1+tree_height t2)
        |LEAF _->0
        )

let tree_to_array t=
        let rec xmdf(a,(x,y))=
                (if array_get(a,(x,y))='|' then xmdf((array_mdf(a,(x+1,y),'-')),(x+1,y))
                else if array_get(a,(x,y))='-' then (
                        if array_get(a,(x+1,y))='|' then a
                        else xmdf((array_mdf(a,(x+1,y),'-')),(x+1,y)))  
                else xmdf(a,(x+1,y))
        )in      
        let rec ymdf(a,(x,y))=(if y>0 then
                ymdf((array_mdf(a,(x,y),'|')),(x,y-1))
                else if y=0 then (array_mdf(a,(x,y),'|'))
                else a) in
        let rec dtn t=
                let rec wid t=(match t with
        NODE(t1,t2)->wid(t1)+wid(t2)+dtn(t)|LEAF(_)->1) in
                let rec top t=(match t with
                NODE(t1,t2)->(top(t1)+top(t2)+wid(t1)+dtn(t))/2
                |LEAF _->0
                ) in
                (match t with
                NODE(t1,t2)->1+((top(t1)+top(t2)+wid(t1)+1) mod 2)
                |LEAF _->0
        ) in
        let rec wid t=(match t with
        NODE(t1,t2)->wid(t1)+wid(t2)+dtn(t)|LEAF(_)->1) in
        let rec top t=(match t with
                NODE(t1,t2)->(top(t1)+top(t2)+wid(t1)+dtn(t))/2
                |LEAF _->0
        ) in
        let rec main (t,a,(x,y))=(match t with
        NODE(t1,t2)->array_mdf(xmdf(main(t2,(main(t1,a,(x,y-1))),(x+wid(t1)+dtn(t),y-1)),(x,y-1)),(x+top(t),y),'|')
        |LEAF(_)->ymdf(a,(x,y))
        ) in
        main(t,array_make(wid(t),(tree_height(t)+1)),(0,tree_height(t)))

let pptree
t=print_char('.');print_newline();if
        array_get(List.rev(tree_to_array(t)),(0,0))!='|' then
                array_print(array_mdf(List.rev(tree_to_array(t)),(0,0),'.'))
else array_print(List.rev(tree_to_array(t)))
(*
TEST CODE
let l=LEAF Korea
let t=NODE(l,l)
let tt=NODE(t,t)
let tl=NODE(t,NODE(tt,l))
let s=NODE(tl,t)
let ss=NODE(l,s)
let sss=NODE(tl,NODE(tt,ss))
let n=NODE(sss,NODE(s,sss))
let nn=NODE(l,NODE(n,tl))
*)
