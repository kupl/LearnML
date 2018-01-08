type team = Korea | France | Usa | Brazil | Japan | Nigeria | Cameroon
| Poland | Portugal | Italy | Germany | Sweden | England
| Croatia | Argentina
type tourna = LEAF of team
| NODE of tourna * tourna

let array_rev a=
        let len=(Array.length a) in
        let rec main a n=(
        let temp=a.(n) in
        if n<((len-1)/2) then ((Array.set a n a.(len-1-n));(Array.set
        a (len-1-n) temp);(main a (n+1)))
        else ()
        ) in
        main a 0
let array_print a=
        let xlen=(Array.length a.(0)) in
        let ylen=(Array.length a) in
        for j=0 to ylen-1 do
                (for i=0 to xlen-1 do print_char(a.(j).(i)) done);
                print_newline()
        done

let rec tree_height t=(match t with 
        NODE(t1,t2)->let h1=tree_height t1 in let h2=tree_height t2 in
                if h1>h2 then (1+h1) else (1+h2)
        |LEAF _->0
        )

let tree_to_array t=
        let rec xmdf(a,(x,y))=
                let c=a.(y).(x) in
                (if c='|' then ((Array.set a.(y) (x+1) '-');xmdf(a,(x+1,y)))
                else if c='-' then (
                        if a.(y).(x+1)='|' then ()
                        else ((Array.set a.(y) (x+1) '-');xmdf(a,(x+1,y)))) 
                else xmdf(a,(x+1,y))
        )in
        let rec ymdf(a,(x,y))=
                (if y>0 then ((Array.set a.(y) x '|');(ymdf(a,(x,y-1))))
                else if y=0 then (Array.set a.(y) x '|')
                else ()) in
        let rec dtn t=(match t with
                NODE(t1,t2)->1+((top(t1)+top(t2)+wid(t1)+1) mod 2)
                |LEAF _->0
        ) 
        and wid t=(match t with
        NODE(t1,t2)->wid(t1)+wid(t2)+dtn(t)|LEAF(_)->1) 
        and top t=(match t with
                NODE(t1,t2)->(top(t1)+top(t2)+wid(t1)+dtn(t))/2
                |LEAF _->0
        ) in
        let h=tree_height(t) in
        let tree_array=(Array.make_matrix (h+1) (wid(t)) ' ') in

        let rec main (t,a,(x,y))=(match t with
        NODE(t1,t2)->(main(t1,a,(x,y-1));main(t2,a,(x+wid(t1)+dtn(t),y-1));
                xmdf(a,(x,y-1));(Array.set a.(y) (x+top(t)) '|'))
        |LEAF(_)->ymdf(a,(x,y))
        ) in
        (main(t,tree_array,(0,h));tree_array)

let pptree t=
        let a=tree_to_array(t) in
        array_rev(a);
        if a.(0).(0)!='|' then ((Array.set a.(0) 0 '.');array_print(a))
        else array_print(a)
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

