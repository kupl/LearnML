type team = Korea | France | Usa | Brazil | Japan | Nigeria | Cameroon | Poland | Portugal | Italy | Germany | Sweden | England | Croatia | Argentina

type tourna = LEAF of team | NODE of tourna * tourna

let max(a,b) = 
	if(a>b) then a
	else b

let rec depth(t) = 
	match t with
	| LEAF(a) -> 1
	| NODE(a,b) -> max(depth(a),depth(b))+1

let rec print_nchar(c, n) = 
	if(n <= 0) then print_string("")
	else if(n == 1) then print_char(c)
	else (print_char(c);print_nchar(c,n-1))

let rec pow(a, b) = 
	if( b<0) then 0
	else if( b== 0) then 1
	else a*pow(a,b-1)

let al(l, c) = List.append l [c]
let all(l, c, d) = List.append (List.append l [c]) [d]

	(* 트리, 맨왼쪽인지, 맨오른쪽인지, 끝난건지 *)
let rec p(now_tree, next_tree, now_depth, max_depth) = 
	match now_tree with
		[] -> next_tree
		| hd::ta -> 
			let (t, dir, ll, rr, e) = hd in
			(
			 	(
				 if (ll == 1) then print_nchar(' ',pow(2,max_depth-now_depth)-1) (* 맨 왼쪽 *)
				 else if(dir == 0) then print_nchar(' ',pow(2,max_depth-now_depth+1)-1) (*걍 왼쪽 *)
				 else if(dir == 1) then print_nchar('-',pow(2,max_depth-now_depth+1)-1) (* 걍 오른쪽 *)
				);
				(
				 	if(e==0) then print_char('|')
					else (print_nchar(' ',e);print_char('|');print_nchar(' ',e))
				);
				(
				 if (rr == 1) then print_char('\n')
				); (* 출력 끝 다음것 처리 *)
				(
				 	match t with 
					| LEAF(k) -> (
						if(e==0) then p(ta, al(next_tree, (LEAF(k), 0, ll, rr, pow(2,max_depth-now_depth-1))), now_depth, max_depth)
						else p(ta, al(next_tree, (LEAF(k), 0, ll, rr, e + pow(2,max_depth-now_depth-1))), now_depth, max_depth)
						)
					| NODE(a,b) -> (
					 	p(ta, all(next_tree, (a, 0, ll, 0, 0), (b, 1, 0, rr, 0)), now_depth, max_depth)
					)
				)
			)

let rec pp(t, now_depth, max_depth) = (* pp(node list, now_depth, max_depth *) 
	if(now_depth > max_depth) then print_string("") 
	else
		pp(p(t, [], now_depth, max_depth), now_depth+1, max_depth)

let pptree(t) = 
	pp([(t,-1,1,1,0)], 1, depth(t))
		
	
(*
 맨 왼쪽 띄어쓰기 -> 2^(n-i)-1 개
 맨 오른쪽 찍고 -> 엔터

 각 왼쪽 자식을 찍을땐 띄어쓰기를 2*(n-i+1)-1개씩 
 각 오른쪽 자식을 찍을땐 -를 2*(n-i+1)-1개씩 

 끝났다면 왼쪽찍고 2^(n-i)공백 찍고 | 찍고 2^(n-i)공백 찍기


 i=1부터 
 왼쪽자식은 (n-i)


 왼쪽 노드 옆에는 띄어쓰기가 있고 오른쪽 노드 옆에는 하이픈이 있다.
 각 왼쪽노드 옆에 띄어쓰기 개수는 2^(n-i+1)-1 이다. 예외로 맨 왼쪽노드는 2^(n-i)-1
 각 오른쪽 노드 옆에 하이픈 개수는 2^(n-i+1)-2 이다. 예외로 맨 오른쪽 노드에는 엔터키

*******|
***|-------|
*|---|***|---|
|-|*|-|*|-|*|-|

*******|
***|-------|
*|---|*****|
|-|*|-|****|

2^(n-i)

***************|
*******|---------------|
***|-------|*******|-------|
*|---|***|---|***|---|***|---|
|-|*|-|*|-|*|-|*|-|*|-|*|-|*|-|


4 6 7
 2 1

 2^(n-e) + 2^(n-i)-1

 *)
