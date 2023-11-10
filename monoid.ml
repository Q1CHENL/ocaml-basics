(*
   In this exercise, we will implement monoids as modules in OCaml. A monoid groups together
   a type t, an associative binary operation plus, and an identity element zero. That means
   that:
   plus (plus x y) z is equal to plus x (plus y z)
   plus zero x and plus x zero both return x
*)

(* given signature: *)
module type Monoid = sig
  type 'a t

  val zero : 'a t
  val plus : 'a t -> 'a t -> 'a t
end

(* TODO: 
1. direct definition
2. Hide parts of a module using module type
  *)


module ListMonoid : Monoid with type 'a t = 'a list = struct
  type 'a t = 'a list

  let zero = []
  let plus l1 l2 = l1 @ l2
end

let example_listmonoid = ListMonoid.plus [ "a"; "b" ] [ "c"; "d"; "e" ]

module FunctionMonoid : Monoid with type 'a t = 'a -> 'a = struct
  type 'a t = 'a -> 'a

  let zero x = x
  let plus f g x = f (g x)
end

let ex_fun_1 = FunctionMonoid.zero "a"
let ex_fun_2 = FunctionMonoid.plus (fun x -> "f " ^ x) (fun x -> "g " ^ x) "y"


(* TODO: module PairListFlippedListMonoid *)
module PairListFlippedListMonoid : Monoid with type 'a t = 'a list * 'a list = struct
  type 'a t = 'a list * 'a list

  let zero = ([], [])
  let plus pair1 pair2 = ((fst pair2) @ (fst pair1), (snd pair1) @ (snd pair2))
end

let ex_plf = PairListFlippedListMonoid.plus (["a"], ["x"; "y"]) (["b"; "c"], ["z"])
