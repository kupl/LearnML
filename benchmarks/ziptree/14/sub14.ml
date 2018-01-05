(* 프로그래밍언어 HW2 Exercise 3
   2009-11657 김동현 *)

type item = string

type tree = LEAF of item
          | NODE of tree list

type zipper = TOP
            | HAND of tree list * zipper * tree list

type location = LOC of tree * zipper

exception NOMOVE of string
(* 왼쪽 끝에서 goLeft, 오른쪽 끝에서 goRight, 꼭대기에서 goLeft/goRight/goUp, 말단에서 goDown 시 *)
exception InvalidTree
(* 노드가 두 개의 부모를 갖는 등 올바르지 않은 형태의 트리 *)

(*
   left - l - t - right  ->  left - l - t - right
              ^                     ^
*)
let goLeft loc = match loc with
    LOC(t, TOP) -> raise (NOMOVE "left of top")
  | LOC(t, HAND(l::left, up, right)) -> LOC(l, HAND(left, up, t::right))
	| LOC(t, HAND([],up,right)) -> raise (NOMOVE "left of first")

(*
   left - t - r - right  ->  left - t - r - right
          ^                             ^
*)
let goRight loc = match loc with
| LOC(t, TOP) -> raise (NOMOVE "right of top")
| LOC(t, HAND(left, up, r::right)) -> LOC(r, HAND(t::left, up, right))
| LOC(t, HAND(left, up, [])) -> raise (NOMOVE "right of first")

(* 
        +
      /   \
     *     *
    / \   / \
   a   b c   d
*)

(* goDown은 왼쪽 아래로 내려가는 것으로 정의 *)
let goDown loc = match loc with
| LOC(LEAF l, z) -> raise (NOMOVE "Cannot go down any more")
| LOC(NODE n, z) -> (match n with
                    | [lc; LEAF ro; rc] -> LOC(lc, HAND([], z, LEAF ro::[rc]))
                    | _ -> raise InvalidTree)
(* | _ -> raise InvalidTree (* Is this pattern match used? *) *)

(*
| LOC(NODE n, TOP) -> (*
                      let [lc; LEAF ro; rc] = n in (* n이 이러한 트리의 경우가 아닌 경우 예외 처리 필요? *)
                      LOC(lc, HAND([], TOP, ro::rc))
                        (* lc, rc는 LEAF일 수도, NODE(leaf가 아닌 subtree)일 수도 있음, 존재하지 않는 경우 [] - NODE에 포함 *)
			                  (* lc, rc가 각각 LEAF인 경우와 NODE인 경우 따로 처리해야?(접합 관련 type 문제) *)
		                  *)
                      (match n with
                      | [lc; LEAF ro; rc] -> LOC(lc, HAND([], TOP, ro::rc))
                      | _ -> raise InvalidTree)
| LOC(NODE n, HAND([], up, rs)) -> (*
                                   let [lc; LEAF ro; rc] = n in
                                   LOC(lc, HAND([], HAND([], up, rs), ro::rc))
                                   *)
                                   (match n with 
                                   | [lc; LEAF ro; rc] -> LOC(lc, HAND([], HAND([], up, rs), ro::rc))
                                   | _ -> raise InvalidTree)
| LOC(NODE n, HAND(ls, up, [])) -> (*
                                   let [lc; LEAF ro; rc] = n in
                                   LOC(lc, HAND([], HAND(ls, up, []), ro::rc))
				                           *)
                                   (match n with
                                   | [lc; LEAF ro; rc] -> LOC(lc, HAND([], HAND(ls, up, []), ro::rc))
				   | _ -> raise InvalidTree
*)

(* 
        +
      /   \
     *     *
    / \   / \
   a   b c   d
*)

let goUp loc = match loc with
| LOC(t, TOP) -> raise (NOMOVE "Cannot go up any more")
(*
| LOC(t, HAND([], z, r::rs)) -> LOC(t::r::rs, z)
| LOC(t, HAND(l::ls, z, [])) -> LOC(ls::l::t, z)
*)
| LOC(t, HAND([], z, r::rs)) -> let [rse] = rs in
                                LOC(NODE[t; r; rse], z)
| LOC(t, HAND(l::ls, z, [])) -> let [lse] = ls in
	                              LOC(NODE[lse; l; t], z)
| _ -> raise InvalidTree

(* 접합시(::, @..) type check 필요: 원소-원소/원소-리스트/리스트-리스트 *)
