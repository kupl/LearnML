(*
    컴퓨터 공학부/ 2007-11740 / 억털괴 / Homework 1
*)


//Exercise 7 2친수의 합
type crazy2 = NIL | ZERO of crazy2 | ONE of crazy2 | MONE of crazy2

fun crazy2add(a, b) =
let
    fun sub x =
    case x of NIL => NIL
    | ZERO(k) => k
    | ONE(k)  => k
    | MONE(k) => k
    
    fun ssum x =
    case x of (NIL, k) => k
    | (k, NIL) => k
    |(ZERO(_), k) => k
    | (k, ZERO(_)) => k
    | _ => ZERO(NIL)

    fun scar x =
    case x of (ONE(_), ONE(_)) => ONE(NIL)
    | (MONE(_), MONE(_)) => MONE(NIL)
    | _ => ZERO(NIL)

    fun opt x =
    case x of NIL => (fn t => t)
    | ZERO(_) => (fn t => ZERO(t))
    | ONE(_)  => (fn t => ONE(t))
    | MONE(_) => (fn t => MONE(t))

    fun cadd (x as (n, m, l)) =
    let
        val fs = ssum(n, m)
        val fc = scar(n, m)
        val ss = ssum(fs, l)
        val sc = ssum(fc, scar(fs, l))
    in
        case x of (NIL, NIL, ZERO(_)) => NIL
        | (NIL, NIL, c) => c
        | _ => (opt(ss) (cadd(sub n, sub m, sc)))
    end
in
    cadd(a, b, NIL)
end
