let f2 x y = function z -> x :: y


let f2 a =
	if a > 10 then 0 else "daldmjla";;

let f1 f x = f x;;
let f a = float(a);;
f1 f 12;;
let rec  f1 x = raise Not_found
(*
let sumProd list = 
	List.fold_left (fun (s, p) h -> 
	(s + h, p * h)) (0, 1) list;;

sumProd([1;2;3;4;5]);;
*)
(*
let rec quicksort' = function
	| [] -> []
	|  x::xs -> let small = List.filter (fun y -> y < x ) xs
and large = List.filter (fun y -> y > x ) xs
in quicksort' small @ (x :: quicksort' large);;

quicksort'([7;3;5;77;11]);;

match(number, list1) with
		| (numb, []) -> [numb]
		| 	(numb, h :: t) -> if numb < h then (numb :: (h :: t)) else h :: insert2(numb,t)
*)

let rec insertSort(orygList) = 
	
		
	let rec insert2(number, list1) = 
		match(number, list1) with
		| (numb, []) -> [numb]
		| 	(numb, h :: t) -> if numb < h then (numb :: (h :: t)) else h :: insert2(numb,t)
		
	
	and insertionSort(list3, list4) = 
		match list3 with
		| [] -> list4
		| h :: t -> insertionSort(t, insert2(h, list4))	
		
		
		in insertionSort(orygList, []);;
	
insertSort([7;3;5;77;11]);;

(*
let insertionsort order list =
	let rec insert element newList = function
	[] -> [element]
	| h :: t as newList -> if not(order h element) then element :: newList
		else h :: insert (element t)

in List.fold_left (fun acc newElement -> insert newElement acc) [] list;;


let myOrder a b =
	fst a <= fst b;;
*)

let rec insertSort(order, list) = 

  let rec insertionSortPrime(xs, lst)  = 
       match xs with 
			| [] -> lst
			| x :: xsd -> insertionSortPrime(xsd, insert(x, lst))
      
  and insert (numb, lst2) = 
        match (numb, lst2) with
				|  (numb, []) -> [numb]
				| (numb, (x :: xs)) ->
          if (order(numb,x)) then numb :: x :: xs
          else x :: insert(numb, xs)

      
  
   in insertionSortPrime(list, []);;
  
let order (num1,num2)= 
    if num1 <= num2 then true else false
      
		;;
  
insertSort(order,[7;3;5;77;11]);;

(*
let insertionsort order list =
	let rec insert element newList = function
	[] ->[element]
	| h :: t as newList -> 
		if not(order h element) then element :: newList else h :: insert element t

	in List.fold_left (fun acc newElement -> insert newElement acc) [] list;;


let myOrder a b = 
	fst a <= fst b;;
		
*)

type 'a bt= Empty | Node of 'a * 'a bt * 'a bt;; 
let tt = Node(1, 
							Node(2, 
											Node(4, 
														Empty, 
														Empty),
											Empty),
							Node(3,
											Node(5,
														Empty,
														Node(6,
																		Empty,
																		Empty)
														),
											Empty
											)
							);;
let breadthBT tree =
	let rec helper = function
		[] -> []
		| [] :: tail -> helper tail
		| Node(value, leftSubtree, rightSubtree) :: tail -> (value :: helper (leftSubtree ::rightSubtree :: tail))
in helper [tree];;


breadthBT(tt);;



let f2 x y = function z -> x::y;;
	
	
type 'a bt= Empty | Node of 'a * 'a bt * 'a bt;;
 
let tt = Node(1, 
							Node(2, 
											Node(4, 
														Empty, 
														Empty),
											Empty),
							Node(3,
											Node(5,
														Empty,
														Node(6,
																		Empty,
																		Empty)
														),
											Empty
											)
							);;



let breadthBT tree =
	let rec helper = function
		[] -> []
		| Empty :: tail -> helper tail
		| Node(value, leftSubtree, rightSubtree) :: tail -> value :: helper (tail @ [leftSubtree;rightSubtree])
in helper [tree];;


breadthBT(tt);;
type 'a graph = Graph of ('a -> 'a list);;


let g = Graph(function 0 -> [3]
| 1 -> [0;2;4]
| 2 -> [1]
| 3 -> []
| 4 -> [0;2]
| n -> failwith ("Graph g: node "^string_of_int n^" doesn't exist"));;

let depthSearch (Graph succ) startNode =
	let rec search visited = function
	[] -> []
	| h :: t -> if List.mem h visited then search visited t
							else h :: search (h :: visited) (succ h @ t)
	in search [] [startNode];;


depthSearch g 4;;


let rec mergesort order list =
	let partition = List.length list / 2 in
	if partition == 0 then list
	else
		let rec merge = function
			[], list2 -> list2
			| list1, [] -> list1
			| head1 :: tail1 as list1, (head2 :: tail2 as list2) ->
				if order head1 head2 then head1 :: merge(tail1, list2)
				else head2 :: merge(list1, tail2)
		and split leftSublist rightSublist counter =

			if counter = 0 then (List.rev leftSublist, rightSublist)
			else split (List.hd rightSublist :: leftSublist) (List.tl rightSublist) (counter - 1)

		in let (left, right) = split [] list partition
in merge(mergesort order left, mergesort order right);;


		