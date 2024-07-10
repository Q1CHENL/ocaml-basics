(* Fold *)

(* There are 2 main built-in folding functions *)
(* 1. fold_left *)
(* to remember the argument order: acc is on the left side of l 
   because it folds from the left *)
let rec fold_left f accu l =
  match l with
  | [] -> accu
  | a::l -> fold_left f (f accu a) l
(* val fold_left : ('acc -> 'a -> 'acc) -> 'acc -> 'a list -> 'acc *)
(* It goes through the given list from left (b1) to right (bn), thus the name *)
(* fold_left f init [b1; ...; bn] is f (... (f (f init b1) b2) ...) bn. *)

(* 2. fold_right *)
(* to remember the argument order: acc is on the right side of l 
   because it folds from the right *)
let rec fold_right f l accu =
  match l with
  | [] -> accu
  | a::l -> f a (fold_right f l accu)
(* val fold_right : ('a -> 'acc -> 'acc) -> 'a list -> 'acc -> 'acc *)
(* It goes through the given list from right (an) to left (a1) *)
(* But since we cannot directly start from the last element, so we have to first go
   deep into the recursion by calling on the tail of the list to reach the last element, 
   thus not tail recursive *)
(* Hence the main difference to fold_left is that, in fold_left, f is applied in every step,
   while in fold_right, it goes deep to reach the last element first, then f is applied 
   layer by layer out till the first layer *)
(* fold_right f [a1; ...; an] init is f a1 (f a2 (... (f an init) ...)). Not tail-recursive. *)

(* Define f1, such that fold_left f1 0 l returns the length of list l *)
let f1 acc v = acc + 1

(* Define f2, such that fold_left f2 [] [l1; l2; ...; ln], for li being 
   arbitrary lists, returns the longest of those lists. If multiple 
   lists have maximal length, any of them may be returned. *)
let f2 acc v = if List.length v > List.length acc then v else acc

(* Define f3, such that fold_left f3 [] [(a1,b1); (a2,b2); ...; (an,bn)] 
   for arbitrary ai, bi computes [(b1,a1); (b2,a2); ...; (bn,an)]. *)
let f3 acc v =
  let a, b = v in
  acc @ [ (b, a) ]

(* let mix lst =
   List.fold_left (fun acc x -> x::(List.rev acc)) [] lst *)
(* Define f4, such that fold_left f4 [] [a0; a1; a2; ...; an-1] for 
   arbitrary elements ai computes [an-1; an-2; ...; a0]. *)
let f4 acc v = v :: List.rev acc

(* Define f5, such that fold_left f5 (fun _ -> ...) [(k1,v1); (k2,v2); ...; (kn,vn)], 
   where (fun _ -> ...) is an arbitrary function, computes a function g such that 
   g(ki) = vi for all 1 ≤ i ≤ n. Assume that the ki are all distinct: ∀ 1 ≤ i < j ≤ n. ki ≠ kj. 
   Your implementation must not call the initial accumulator (fun _ -> ...). *)
let f5 acc v =
  let ki, vi = v in
  fun k -> if ki = k then vi else acc k

(* Define f6, such that fold_left f6 [v] [f1; f2; ...; fn] computes 
   [fn(...(f2(f1(v)))...); ...; f2(f1(v)); f1(v); v] for unary 
   functions fi and arbitrary v. *)
let f6 acc v = v (List.hd acc) :: acc

(* Define f7, such that fold_left f7 a [cn; cn-1; ...; c0] computes 
   a2^n+1 * ∏(from i=0 to n) ci^2^i for integers a, c0, c1, ..., cn. *)
let f7 acc v = acc * acc * v
let res = List.fold_left f7 (-1) [ 2; -4; 58; -7; -2 ]
let res2 = List.fold_left f7 (-3) [ 6; -2; 3 ]

(* For more custom fold function like foldr_i_opt and foldr_i_opt_tr please
   refer to tailrec.ml *)