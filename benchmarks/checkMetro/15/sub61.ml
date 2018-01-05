(*2-3 컴공 2014-10618 이세영*)
type metro=STATION of name
          |AREA of name*metro
          |CONNECT of metro*metro
and name=string;;
let checkMetro met=
    let rec checkList li met=
        match met with
        |STATION a->let rec find=function
                        |[]->false
                        |x::li->if x=a then true else find li 
        in find li
        |AREA (x,y)->if (checkList (x::li) y)=true then true else false
        |CONNECT (x,y)-> if (checkList li x)=true && (checkList li y)=true then true else false
    in (checkList [] met);;
