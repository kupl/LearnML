(*2-4 컴공 2014-10618 이세영*)
type item=string;;
type tree=LEAF of item | NODE of tree list;;
type zipper= TOP | HAND of tree list*zipper*tree list;;
type location=LOC of tree * zipper;;
exception NOMOVE of string;;
let goLeft loc= match loc with
    |LOC(t, TOP)->raise (NOMOVE "left of top")
    |LOC(t, HAND(l::left, up, right))->LOC(l, HAND(left, up, t::right))
    |LOC(t, HAND([],up,right))->raise (NOMOVE "left of first");;
let goRight loc=match loc with
    |LOC(t, TOP)->raise (NOMOVE "right of top")
    |LOC(t, HAND(left, up, r::right))->LOC(r, HAND(t::left,up,right))
    |LOC(t, HAND(left,up,[]))->raise( NOMOVE "right of first");;
let rec merge(left, right)= match left with
    |[]->right
    |l::le->merge(le,l::right);;
let goUp loc=match loc with
    |LOC(t, TOP)->raise (NOMOVE "top")
    |LOC(t, HAND(left,up,right))->LOC(NODE(merge(left,t::right)),up);;
let firstmember t=match t with
    |[]->raise (NOMOVE "bottom")
    |x::li->x;;
let othermember t=match t with
    |[]->raise (NOMOVE "bottom")
    |x::li->li;;
let goDown loc=match loc with
    |LOC(t,x)-> match t with
                |LEAF t->raise (NOMOVE "bottom")
                |NODE t->LOC(firstmember t,HAND([],x,othermember t));;

