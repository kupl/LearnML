(*2-3 컴공 2014-10618 이세영*)
type lambda=V of var
          |P of var*lambda
          |C of lambda*lambda
and var=string;;
let check met=
    let rec checkList li met=
        match met with
        |V a->let rec find=function
                        |[]->false
                        |x::li->if x=a then true else find li 
        in find li
        |P (x,y)->if (checkList (x::li) y)=true then true else false
        |C (x,y)-> if (checkList li x)=true && (checkList li y)=true then true else false
    in (checkList [] met);;
