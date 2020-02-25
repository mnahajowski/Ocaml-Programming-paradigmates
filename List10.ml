(*
module type QUEUE_MUT =
sig
type 'a t
(* The type of queues containing elements of type ['a]. *)
exception Empty of string
(* Raised when [first q] is applied to an empty queue [q]. *)
exception Full of string
(* Raised when [enqueue(x,q)] is applied to a full queue [q]. *)
val empty: int -> 'a t
(* [empty n] returns a new queue of length [n], initially empty. *)
val enqueue: 'a * 'a t -> unit
(* [enqueue (x,q)] adds the element [x] at the end of a queue [q]. *)
val dequeue: 'a t -> unit
(* [dequeue q] removes the first element in queue [q] *)
val first: 'a t -> 'a
(* [first q] returns the first element in queue [q] without removing
it from the queue, or raises [Empty] if the queue is empty. *)
val isEmpty: 'a t -> bool
(* [isEmpty q] returns [true] if queue [q] is empty,
otherwise returns [false]. *)
val isFull: 'a t -> bool
(* [isFull q] returns [true] if queue [q] is full,
otherwise returns [false]. *)
end;;
*)
(*
module Queue_mut: QUEUE_MUT =
struct
	type 'a t = {mutable f:int; mutable r:int; mutable a: 'a option array }
	exception Empty of string
	exception Full of string
	let empty n = { f=0; r=0; a=Array.make (n+1) None }
	let enqueue (el, arr) =
	if (arr.r+1) mod (Array.length arr.a)=arr.f then raise (Full "enqueue")

					else begin
							arr.a.(arr.r) <- Some el;
					arr.r <- (arr.r+1) mod (Array.length arr.a)
					end
	let dequeue arr =
		if arr.f!=arr.r then arr.f <- (arr.f+1) mod (Array.length arr.a)
	let first arr =

			if arr.f=arr.r then raise (Empty "first")
			else match arr.a.(arr.f) with
					Some e -> e
				| None -> failwith "To i tak sie nigdy nie zdarzy"

	let isEmpty arr = arr.f=arr.r
	let isFull arr = (arr.r+1) mod (Array.length arr.a)=arr.f
end;;
*)

module CyclicArrayQueue : QUEUE_MUT =
	struct
		type 'a t = { a : 'a option array; mutable f: int; mutable r: int }
		exception Empty of string
		exception Full of string
		
		let empty n =
			{ a = Array.make (n + 1) None; f = 0; r = 0 }
			
		let isEmpty q =
			q.r = q.f
			
		let isFull q =
			q.r - q.f = -1 || q.r - q.f = Array.length q.a - 1
			
		let enqueue (element, q) =
			if (isFull q) then raise (Full "module CyclicArrayQueue: enqueue")
			else
				q.a.(q.r) <- Some element;
				if q.r = Array.length q.a - 1 then q.r <- 0
				else q.r <- succ(q.r)
				
				
		let dequeue q =
			if (isEmpty q) then ()
			else
			if q.f = Array.length q.a - 1 then q.f <- 0
			else q.f <- succ(q.f)
			
		let first q =
			if (isEmpty q) then raise (Empty "module CyclicArrayQueue: first&quot")
			else match (q.a.(q.f)) with
				| Some value -> value
				| None -> failwith "module CyclicArrayQueue: first (implementation
error!!!)"
	
end;;

module CyclicArrayQueue : QUEUE_MUT = StackList;;










