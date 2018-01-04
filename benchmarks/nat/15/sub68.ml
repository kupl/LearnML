(* 2014-18790 JangHo Seo <jangho.se@snu.ac.kr>
 * Programming Languages 2015 Fall
 * Homework 1, Exercise 5 *)

type nat = ZERO | SUCC of nat

let rec natadd (n1, n2) =
    match n2 with
    | ZERO -> n1
    | SUCC n2minusOne -> natadd (SUCC n1, n2minusOne)

let rec iter_natadd result operand count =
    match count with
    | ZERO -> result
    | SUCC countMinusOne -> iter_natadd (natadd (result, operand)) operand countMinusOne

let natmul (n1, n2) = iter_natadd ZERO n1 n2
