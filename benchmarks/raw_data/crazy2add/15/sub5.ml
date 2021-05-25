(* SNU Programming Language Fall 2015
 * Homework 2 
 * Exercise 2: crazy2add
 * Written by Dongho Kang 
 *)

type crazy2 = NIL 
| ZERO of crazy2
| ONE of crazy2 
| MONE of crazy2
;;

let rec crazy2add: crazy2 * crazy2 -> crazy2 = fun (num1, num2) ->
    let rec crazy2inc: crazy2 -> crazy2 = fun num ->   
        (* sub-fun: for increasing crazy2 num *)
        match num with
        | NIL            -> ONE NIL
        | MONE  num_n    -> ZERO num_n
        | ZERO  num_n    -> ONE  num_n     
        | ONE   num_n    -> ZERO (crazy2inc num_n)
    in

    let rec crazy2dec: crazy2 -> crazy2 = fun num ->  
        (* sub-fun: for decreasing crazy2 num *)
        match num with
        | NIL            -> MONE NIL
        | MONE  num_n    -> ZERO  (crazy2dec num_n)
        | ZERO  num_n    -> MONE num_n     
        | ONE   num_n    -> ZERO num_n
    in

    (* recursive step *)
    match num2 with 
    | NIL           -> num1 (* if num2 is NIL then result is num1 *)
    | ZERO num2_n   ->      (* if num2 is (0 ...) then... *)
            (match num1 with
            | NIL -> num2
            | ZERO num1_n -> ZERO (crazy2add (num1_n, num2_n))
            | ONE  num1_n -> ONE  (crazy2add (num1_n, num2_n))
            | MONE num1_n -> MONE (crazy2add (num1_n, num2_n)))
    | ONE num2_n    ->      (* if num2 is (+ ...) then... *)
            (match num1 with
            | NIL -> num2
            | ZERO num1_n -> ONE  (crazy2add (num1_n, num2_n))
            | ONE  num1_n -> ZERO (crazy2inc (crazy2add (num1_n, num2_n)))
            | MONE num1_n -> ZERO (crazy2add (num1_n, num2_n)))
    | MONE num2_n   ->      (* if num2 is (- ...) then... *)
            (match num1 with
            | NIL -> num2
            | ZERO num1_n -> MONE (crazy2add (num1_n, num2_n))
            | ONE  num1_n -> ZERO (crazy2add (num1_n, num2_n))
            | MONE num1_n -> ZERO (crazy2dec (crazy2add (num1_n, num2_n))))
;;
