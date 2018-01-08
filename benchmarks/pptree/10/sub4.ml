type team = Korea | France | Usa | Brazil | Japan | Nigeria | Cameroon
| Poland | Portugal | Italy | Germany | Sweden | England
| Croatia | Argentina
;;

type tourna = LEAF of team
| NODE of tourna * tourna
;;

let rec drow_leaf(n, s, l) =
        if n>0 then( 
        l.(n) <- l.(n) ^ s
        ;drow_leaf(n-1,s,l)
        )
;;

let rec drow (n, t, l) =
        let m = int_of_float (2.0**(float_of_int(n-1))) in(
        match t with
        LEAF a -> if not (n=0) then drow_leaf (n, (String.make (2*m-1) ' ' ^ "|" ^ String.make (2*m) ' '), l)
        |NODE(a,b) -> l.(n) <- l.(n) ^ String.make (m-1) ' ' ^ "|" ^ String.make (2*m-1) '-' ^ "|" ^ String.make m ' '
        ;drow(n-1, a, l)
        ;drow(n-1, b, l)
        )
;;

let rec height (n,t) =
        match t with
        LEAF a -> n
        |NODE(a,b) -> if height(n+1, a) > height(n+1, b) then height(n+1, a) else
                height(n+1,b)

let rec printt (h,l)=
        if h>0 then(
        print_string(l.(h)^"\n")
        ;printt (h-1, l)
        )
;;

let pptree t =
        let h = height (0,t) in
        let l = Array.make (h+3) "" in(
        let m = int_of_float (2.0**(float_of_int(h))) in(
        l.(h+1) <- l.(h+1) ^ String.make (m-1) ' ' ^ "|" ^ String.make m ' '
        )
        ;l.(h+2) <- l.(h+2) ^ "result:"
        ;drow(h,t,l)
        ;printt (h+2, l)
        )
;;
