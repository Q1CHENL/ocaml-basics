module type Monoid = sig
  type 'a t

  val zero : 'a t
  val plus : 'a t -> 'a t -> 'a t
end
i


(* TODO: module ListMonoid *)
module ListMonoid : Monoid = struct
  type 'a t = 'a list
  let zero = [] 

  let plus = fun l1 l2 -> l1@l2

end

(* EXAMPLES:
1. ListMonoid.plus ["a"; "b"] ["c"; "d"; "e"] = ["a"; "b"; "c"; "d"; "e"]
*)


(* TODO: module FunctionMonoid *)
module FunctionMonoid : Monoid = struct
  type 'a t = 'a -> 'a
  let zero = fun x -> x

  let plus = fun f g x -> f (g x) 

end

(* EXAMPLES:
1. FunctionMonoid.zero "a" = "a"
2. (FunctionMonoid.plus (fun x -> "f " ^ x) (fun x -> "g " ^ x)) "y" = "f g y"
*)


module type MonoidOperations = functor (M : Monoid) -> sig
  val fold : 'a M.t list -> 'a M.t
  val mul : int -> 'a M.t -> 'a M.t
end

(* TODO: functor MonoidOperations *)
module MonoidOperations = functor (X: Monoid) -> struct
end

(* EXAMPLES:
Given the following module:
  module Ops = MonoidOperations (ListMonoid)
Then:
1. Ops.fold [["a"; "b"]; ["c"]; ["d"; "e"]] = ["a"; "b"; "c"; "d"; "e"]
2. Ops.fold [] = []
3. Ops.mul 3 ["a"; "b"] = ["a"; "b"; "a"; "b"; "a"; "b"]
4. Ops.mul 0 ["a"; "b"] = []
*)


(* TODO: functor FlipMonoid *)
module FlipMonoid = functor (M: Monoid) -> struct
type 'a t = 'a M.t
  let fold = fun x: 'a M.t list -> List.hd x
  let mul = fun (x: int) y: 'a M.t -> y
end

(* EXAMPLES:
Given the following module:
  module F = FlipMonoid (ListMonoid)
Then:
1. F.plus ["a"; "b"] ["c"; "d"; "e"] = ["c"; "d"; "e"; "a"; "b"]
*)


(* TODO: functor OptionMonoid *)
module OptionMonoid = functor (M: Monoid) -> struct
type 'a t = 'a M.t option
let fold = fun x: 'a M.t list -> List.hd x
let mul = fun (x: int) y: 'a M.t -> y
end

(* EXAMPLES:
Given the following module:
  module O = OptionMonoid (ListMonoid)
Then:
1. O.plus None None = None
2. O.plus (Some ["a"; "b"]) None = Some ["a"; "b"]
3. O.plus (Some ["a"; "b"]) (Some ["c"]) = Some ["a"; "b"; "c"]
*)


(* TODO: functor PairMonoid *)
module PairMonoid = functor (L: Monoid) (R: Monoid) -> struct
  type 'a t = ('a L.t * 'a R.t)
  let fold = fun x: 'a L.t list -> List.hd x
  let mul = fun (x: int) y: 'a L.t -> y
end

(* EXAMPLES:
Given the following module:
  module P = PairMonoid (ListMonoid) (ListMonoid)
Then:
1. P.plus (["a"], ["x"; "y"]) (["b"; "c"], ["z"])
    = (["a"; "b"; "c"], ["x"; "y"; "z"])
*)


(* TODO: module PairListFlippedListMonoid *)
module PairListFlippedListMonoid : Monoid =  struct
  type 'a t = 'a list * 'a list
  let zero = ([],[])
  let plus = fun x y -> ([], [])
end

(* EXAMPLES:
1. PairListFlippedListMonoid.plus (["a"], ["x"; "y"]) (["b"; "c"], ["z"])
    = (["a"; "b"; "c"], ["z"; "x"; "y"])
*)
