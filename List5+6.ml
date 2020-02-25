type 'a llist = LNil | LCons of 'a * 'a llist Lazy.t;;

let lrepeat k llist =
	let rec helper(reps, rest) = match (reps, rest) with
		| (_, LNil) -> LNil
		| (0, LCons(_, tail )) -> helper(k, function() -> tail() Lazy.t)
		| (_, LCons(head, _)) -> LCons(head, function() -> helper(reps - 1, rest))
	in helper(k, llist);;

(*
let rec fibs =
  Cons(1, fun () ->
    Cons(1, fun () ->
      sum fibs (tl fibs)))
			*)
			


ltake(5,lfrom 30);;

let rec lsquares= function 
	| LNil-> LNil
	| LCons(x,lazy xs) -> LCons(x*x, lazy(lsquares xs));;

ltake(6, lsquares(lfrom 3));;

ltake(6, lfrom 3);;




let rec lrepeat (k, llist) = 
	let rec helper (reps, rest) = match (reps, rest) with
	| (_, LNil) -> LNil
	| (0, LCons(_, lazy xs)) -> helper(k, xs)
	| (_, LCons(x, _)) -> LCons(x, lazy(helper(reps -1, rest)))
in helper(k, llist);;

ltake(6, lrepeat(2,lfrom 30));;


let lfib =
	let rec fibgen a b =
	LCons(a, lazy(fibgen b (a + b)))
in fibgen 1 1;;

ltake(6, lfib);;

			(*			
let rec lrepeat k llist =
  let rec repeatElement(element, reps, rest) =
  	if reps = 0 then rest
  	else repeatElement(element, reps - 1, LCons(element, function () -> rest))
  	in match llist with
  		| LNil ->; LNil
  		| LCons(firstElement, tailFunction) -> repeatElement(firstElement, k, (lrepeat k (tailFunction())));;
			
let rec ltake= 
	function(0, _)                -> []
	| (_, LNil)             -> []
	| (n, LCons(x,lazy xs)) -> x::ltake(n-1,xs);;
*)

(*
type 'a llist= LNil| LCons of 'a * (unit -> 'a llist);;

let lhd= function LNil -> failwith"lhd"
	| LCons (x, _) -> x;;

let ltl= function LNil -> failwith"ltl"
| LCons (_, xf) -> xf()

let rec lfrom k = LCons(k, function () -> lfrom(k+1));;

let rec ltake= function(0, _) -> []
| (_, LNil) -> []
| (n, LCons(x,xf)) -> x::ltake(n-1, xf());;

ltake(5,lfrom 30);;


let rec toLazyList= function[]   -> LNil
| h::t -> LCons(h, function() -> toLazyList t);;
*)
type 'a llist = LNil | LCons of 'a * 'a llist Lazy.t;;
type 'a lBT = LEmpty | LNode of  'a * (unit ->'a lBT) * (unit -> 'a lBT);;


let toLazyList tree =
	let rec helper = function
		[] -> LNil
		| LEmpty :: tail -> helper tail
		| LNode(value, leftSubtree, rightSubtree) :: tail -> LCons(value, lazy(helper(tail @ [leftSubtree(); rightSubtree()])))
in helper [tree];;

let rec lTree n =
	LNode(n, (function () -> lTree (2 * n)), function () -> lTree (2 * n + 1));;

let rec ltake= function
	| (0, _)  -> []
	| (_, LNil)             -> []
	| (n, LCons(x,lazy xs)) -> x::ltake(n-1,xs);;

ltake(15, toLazyList(lTree 1));;

