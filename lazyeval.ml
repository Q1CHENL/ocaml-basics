(* Lazy Evaluation *)
(* Infinite data structures (e.g. lists) can be realized using the
   concept of lazy evaluation. Instead of constructing the entire
   data structure immediately, we only construct a small part and
   keep us a means to construct more on demand.

   In terms of the actual implementation, we often defer the evaluation
   of future values using a function that only takes a unit and
   returns a new lazy construct with the next value
*)

(* Relevant exercises: w08h03, w08t02 *)

(* w08h03: lazy list *)
(* The type of the lazy list is a tuple of the head value of the
   lazy list and a function only taking a unit to generate all
   the following values on demand *)
type 'a llist = Cons of 'a * (unit -> 'a llist)

(* constructs the list of all natural numbers starting at the given argument. *)
let rec lnat n = Cons (n, fun () -> lnat (n + 1))

(* Basic usage:
   If we want a list of natural number greater equal 1
   let l = lnat 1
   Console output: val l : int llist = Cons (1, <fun>)
   if we want to calculate the following values, we need
   to get the that <fun> and apply it to a ()
   let Cons (n, f) = l
   Console output:
   val n : int = 1
   val f : unit -> int llist = <fun>
   then let nxt = f ()
   gives us val nxt : int llist = Cons (2, <fun>) *)

(* constructs a list containing the Fibonacci sequence. *)
let rec lfib () =
  let rec impl a b = Cons (a, fun () -> impl b (a + b)) in
  impl 0 1

let a = lfib ()

(* Advanced usage: returns the first n elements of the lazy list as a normal. *)
let rec ltake n (Cons (h, t)) =
  let rec impl n ll stack =
    let (Cons (h, t)) = ll in
    if n > 0 then impl (n - 1) (t ()) (h :: stack) else List.rev stack
  in
  impl n (Cons (h, t)) []

(* Advanced usage: filter those elements from the list that do not satisfy the given predicate. *)
(* Notice that this also returns a lazy list instead of a list.
   It basically passed the predicate down to the future values *)
let rec lfilter p (Cons (h, t)) =
  if p h then Cons (h, fun () -> lfilter p (t ())) else lfilter p (t ())

(* ================================================================================================ *)

(* w08h03: lazy tree *)
(* It builds on a normal tree type, but defer the evaluation of the left and right child tree *)
type 'a tree = Empty | Node of 'a * 'a tree * 'a tree
type 'a ltree = LNode of 'a * (unit -> 'a ltree) * (unit -> 'a ltree)

(* An example of a lazy tree, where all nth nodes stores the n+r value *)
let rec layer_tree r =
  LNode (r, (fun () -> layer_tree (r + 1)), fun () -> layer_tree (r + 1))

(* map all elements in the lazy tree with f *)
let rec map f t =
  let (LNode (lv, fl, fr)) = t in
  LNode (f lv, (fun () -> map f (fl ())), fun () -> map f (fr ()))

(* returns the top n layers of the given infinite tree t as a finite binary tree *)
let top n t =
  let rec impl n t ft =
    if n < 1 then ft
    else
      let (LNode (lv, fl, fr)) = t in
      Node (lv, impl (n - 1) (fl ()) Empty, impl (n - 1) (fr ()) Empty)
  in
  impl n t Empty

let rec find_in_lt_lst p l =
  match l with
  | [] -> []
  | LNode (v, fl, fr) :: xs ->
      if p v then [ LNode (v, fl, fr) ] else find_in_lt_lst p xs

let first_nth_layer_of_lt n t =
  let rec impl cnt n (LNode (lv, fl, fr)) stack =
    if cnt < n then
      impl (cnt + 1) n (fl ()) stack @ impl (cnt + 1) n (fr ()) stack
    else LNode (lv, fl, fr) :: stack
  in
  impl 1 n t []

(* idea: take all lnode of first nth layer and check them all *)
(* we do this iteratively from k = 1 to k = n *)
let find p t =
  let rec impl p t n =
    let nth_layer = first_nth_layer_of_lt n t in
    let found = find_in_lt_lst p nth_layer in
    match found with [] -> impl p t (n + 1) | x :: _ -> x
  in
  impl p t 1
