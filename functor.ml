(* open the module Monoid. If the module name is not the same as the file name,
   e.g Module.ml and module Monoid, then should use "open Module" instead, even though
   open should be followed with a Module name. That is because ocaml then treats
   Moudle.ml as a module and Monoid as a submodule of Module.*)
open Monoid

(* In the topic functor, there are 2 types of things:
   - functor type
   - functor (implementation)
   The relation is just like module type and module (implementation)

   Just view functor as a function, and functor type as function type

          type               ft = int                 -> int
   module type MonoidOperations = functor (M: Monoid) -> sig ... end

                     let f: ft = fun      x          -> x * 2
             module type F: FT = functor (M: Monoid) -> sig ... end

   The argument of both functor and functor type is a module type, not
   a module (implementation)
*)

(* A functor type *)
module type MonoidOperations = functor (M : Monoid) -> sig
  val fold : 'a M.t list -> 'a M.t
  val mul : int -> 'a M.t -> 'a M.t
end

(* Notice: a functor type can only be implemented by a functor (implementation), not another functor type*)
(* module type F: MonoidOperations = functor (M: Monoid) -> sig ... end ===> cannot compile*)

(* A functor (implementation) *)
module MonoidOperations : MonoidOperations =
functor
  (M : Monoid)
  ->
  struct
    let fold lst =
      match lst with
      | [] -> M.zero
      | _ -> List.fold_right (fun x acc -> M.plus x acc) lst M.zero

    let rec mul n x = if n == 0 then M.zero else M.plus x (mul (n - 1) x)
  end

module StandaloneMonoid = functor (M : Monoid) -> struct end
module Ops = MonoidOperations (ListMonoid)

let ex_op_1 = Ops.fold [ [ "a"; "b" ]; [ "c" ]; [ "d"; "e" ] ]
let ex_op_2 = Ops.fold []
let ex_op_3 = Ops.mul 3 [ "a"; "b" ]
let ex_op_4 = Ops.mul 0 [ "a"; "b" ]

module FlipMonoid =
functor
  (M : Monoid)
  ->
  struct
    type 'a t = 'a M.t

    let plus l1 l2 = l2 @ l1
  end

module F = FlipMonoid (ListMonoid)

let ex_flip = F.plus [ "a"; "b" ] [ "c"; "d"; "e" ]

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

module O = OptionMonoid (ListMonoid)

let ex_option_1 = O.plus None None
let ex_option_2 = O.plus (Some [ "a"; "b" ]) None
let ex_option_3 = O.plus (Some [ "a"; "b" ]) (Some [ "c" ])

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

module P = PairMonoid (ListMonoid) (ListMonoid)

let ex_pair = P.plus ([ "a" ], [ "x"; "y" ]) ([ "b"; "c" ], [ "z" ])
