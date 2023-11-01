(*
   In this exercise, we will implement monoids as modules in OCaml. A monoid groups together
   a type t, an associative binary operation plus, and an identity element zero. That means
   that:
   plus (plus x y) z is equal to plus x (plus y z)
   plus zero x and plus x zero both return x
*)

(* given signature:  *)
module type Monoid = sig
  type 'a t

  val zero : 'a t
  val plus : 'a t -> 'a t -> 'a t
end

(* TODO: module ListMonoid *)
module ListMonoid : Monoid with type 'a t = 'a list = struct
  type 'a t = 'a list

  let zero = []
  let plus l1 l2 = l1 @ l2
end

(* EXAMPLES:
   1. ListMonoid.plus ["a"; "b"] ["c"; "d"; "e"] = ["a"; "b"; "c"; "d"; "e"]
*)
(* passed *)
let example_listmonoid = ListMonoid.plus [ "a"; "b" ] [ "c"; "d"; "e" ]

(* TODO: module FunctionMonoid *)
module FunctionMonoid : Monoid with type 'a t = 'a -> 'a = struct
  type 'a t = 'a -> 'a

  let zero x = x
  let plus f g x = f (g x)
end

(* EXAMPLES:
   1. FunctionMonoid.zero "a" = "a"
   2. (FunctionMonoid.plus (fun x -> "f " ^ x) (fun x -> "g " ^ x)) "y" = "f g y"
*)
(* passed *)
let ex_fun_1 = FunctionMonoid.zero "a"
let ex_fun_2 = FunctionMonoid.plus (fun x -> "f " ^ x) (fun x -> "g " ^ x) "y"

module type MonoidOperations = functor (M : Monoid) -> sig
  val fold : 'a M.t list -> 'a M.t
  val mul : int -> 'a M.t -> 'a M.t
end

(* TODO: functor MonoidOperations *)
module MonoidOperations : MonoidOperations =
functor
  (M : Monoid)
  ->
  struct
    let fold lst =
      match lst with
      | [] -> M.zero
      | _ -> List.fold_right (fun x acc -> M.plus x acc) lst M.zero
    (* let fold lst = List.hd lst *)

    let rec mul n x = if n == 0 then M.zero else M.plus x (mul (n - 1) x)
  end

(* EXAMPLES:
   Given the following module:
*)
module Ops = MonoidOperations (ListMonoid)

(* let t1 = Ops.fold [["a"; "b"]; ["c"]; ["d"; "e"]] *)
(*Then:
  1. Ops.fold [["a"; "b"]; ["c"]; ["d"; "e"]] = ["a"; "b"; "c"; "d"; "e"]
  2. Ops.fold [] = []
  3. Ops.mul 3 ["a"; "b"] = ["a"; "b"; "a"; "b"; "a"; "b"]
  4. Ops.mul 0 ["a"; "b"] = []
*)
let ex_op_1 = Ops.fold [ [ "a"; "b" ]; [ "c" ]; [ "d"; "e" ] ]
let ex_op_2 = Ops.fold []
let ex_op_3 = Ops.mul 3 [ "a"; "b" ]
let ex_op_4 = Ops.mul 0 [ "a"; "b" ]

(* TODO: functor FlipMonoid *)
module FlipMonoid =
functor
  (M : Monoid)
  ->
  struct
    type 'a t = 'a M.t

    let plus l1 l2 = l2 @ l1
  end

(* EXAMPLES:
   Given the following module:
*)
module F = FlipMonoid (ListMonoid)

(*
  Then:
1. F.plus ["a"; "b"] ["c"; "d"; "e"] = ["c"; "d"; "e"; "a"; "b"]
*)
let ex_flip = F.plus [ "a"; "b" ] [ "c"; "d"; "e" ]

(* TODO: functor OptionMonoid *)
module OptionMonoid =
functor
  (M : Monoid)
  ->
  struct
    type 'a t = 'a M.t option

    let plus optx opty =
      match (optx, opty) with
      | None, None -> None
      | Some _, None -> optx
      | None, Some _ -> opty
      | Some x, Some y -> Some (M.plus x y)
  end

(* EXAMPLES:
   Given the following module:
*)
module O = OptionMonoid (ListMonoid)
(*
Then:
1. O.plus None None = None
2. O.plus (Some ["a"; "b"]) None = Some ["a"; "b"]
3. O.plus (Some ["a"; "b"]) (Some ["c"]) = Some ["a"; "b"; "c"]
*)

let ex_option_1 = O.plus None None
let ex_option_2 = O.plus (Some [ "a"; "b" ]) None
let ex_option_3 = O.plus (Some [ "a"; "b" ]) (Some [ "c" ])

(* TODO: functor PairMonoid *)
module PairMonoid =
functor
  (L : Monoid)
  (R : Monoid)
  ->
  struct
    type 'a t = 'a L.t * 'a R.t

    let zero = (L.zero, R.zero)
    let plus (l1, r1) (l2, r2) = (L.plus l1 l2, R.plus r1 r2)
  end

(* EXAMPLES:
   Given the following module:
*)
module P = PairMonoid (ListMonoid) (ListMonoid)

(* Then:
   1. P.plus (["a"], ["x"; "y"]) (["b"; "c"], ["z"])
       = (["a"; "b"; "c"], ["x"; "y"; "z"])
*)
let ex_pair = P.plus ([ "a" ], [ "x"; "y" ]) ([ "b"; "c" ], [ "z" ])

(* TODO: module PairListFlippedListMonoid *)
module PairListFlippedListMonoid : Monoid with type 'a t = 'a list * 'a list = struct
  type 'a t = 'a list * 'a list

  let zero = ([], [])
  let plus pair1 pair2 = ((fst pair2) @ (fst pair1), (snd pair1) @ (snd pair2))
end

(* EXAMPLES:
   1. PairListFlippedListMonoid.plus (["a"], ["x"; "y"]) (["b"; "c"], ["z"])
       = (["a"; "b"; "c"], ["z"; "x"; "y"])
*)
let ex_plf = PairListFlippedListMonoid.plus (["a"], ["x"; "y"]) (["b"; "c"], ["z"])
