(* open the module Monoid. If the module name is not the same as the file name,
   e.g Module.ml and module Monoid, then should use "open Module" instead, even though
   open should be followed with a Module name. That is because ocaml then treats
   Moudle.ml as a module and Monoid as a submodule of Module.*)
open Monoid

(* First, what is a functor?
   It's a higher order function which takes a module/functor and returns a module/functor
   In essence, it's really just like function, takes something, do some processing, return
   something. Just that here the arguments and the return values are all module/functors

   In the topic functor, there are 2 types of things:
   - functor type: the type of the functor
   - functor (implementation): the actual implementation of the functor
   The relation is just like module type and module (implementation)

   Just view functor as a function, and functor type as function type

   Comparison of function type and functor type:

          type ft = int                 -> int
   module type FT = functor (M: Monoid) -> sig val f: 'a -> 'a ... end
   This is only way to write a functor type

   Comparison of function implementation and functor implementation:

    1. using keyword fun and functor, its actually a anynomous function/functor in the body
       let f: ft = fun      x          -> x * 2
    module F: FT = functor (M: Monoid) -> struct let f x = x * 2 ... end

    2. direct implementation without keyword fun or functor
       let f (x:    int): int        = x * 2
    module F (M: Monoid): ListMonoid = struct ... end
    Here ": int" and ": List" before the equal sign specifies the return type

   The argument of both functor and functor type is a module type, not
   a module (implementation)

   The arguments of functor/functor type are module type/functor types, not module/functor (implementation)
   This is intuitive
*)
module type Monoid = sig
  type 'a t

  val zero : 'a t
  val plus : 'a t -> 'a t -> 'a t
end

module ListMonoid : Monoid with type 'a t = 'a list = struct
  type 'a t = 'a list

  let zero = []
  let plus l1 l2 = l1 @ l2
end

module IntMonoid : Monoid with type 'a t = int = struct
  type 'a t = int

  let zero = 0
  let plus x y = x + y
end

module type FoldAndMulModuleType = sig 
  type 'a t
  val fold : 'a t list -> 'a t
  val mul : int -> 'a t -> 'a t
end

module type MonoidOperationsFunctorType = functor (M : Monoid) -> FoldAndMulModuleType with type 'a t := 'a M.t

(* A functor type *)
(* Takes an argument of type Monoid, returns a module *)
(* It cannot be directly used to create modules, but rather
   is to be conformed to by a implementation of a functor *)
module type MonoidOperationsFunctorType = functor (M : Monoid) -> sig
  (* include Monoid *)

  val fold : 'a M.t list -> 'a M.t
  val mul : int -> 'a M.t -> 'a M.t
end
(* Or we can do this seperately: first define the type of the module we want to return *)
module type FoldAndMulModuleType = sig 
  type 'a t
  val fold : 'a t list -> 'a t
  val mul : int -> 'a t -> 'a t
end

module type MonoidOperationsFunctorType = functor (M : Monoid) -> FoldAndMulModuleType with type 'a t = 'a M.t


(* Can use = to directly create new types from the old ones *)
(* module type MonoidOperationsFunctorType2 = MonoidOperationsFunctorType *)

(* A functor that conforms to the functor type above *)
(* It returns a module with 2 funtions: fold and mul.
   One misunderstanding may be: it must return a module that has something to do with the argument
   Well yes and no.
   Yes: it will use something defined in M, like the type, or function
   No: it could and often returns a group of completely new functions that are not in M at all

   Remember, functor is very flexible, it's really just like function, returns what we specify,
   not neccessarily include the functions of the argument
*)
(* Compared to the similar function definiation *)
(* Whenever this is argument right after the name of our functor, the thing after : is not the type 
   our functor conforms to, but the return type, which can be a module or a functor, it's similar to
   the function definition below, just that the right hand side fun x y -> x * y is not allowed in function 
   here it return a functor of type MonoidOperationsFunctorType *)
(* let f (x : int) (y : int) : int = fun x y -> x * y *)
module MonoidOperationsFunctor (M: Monoid) : MonoidOperationsFunctorType =
functor
(M : Monoid)
->
  struct
  (* include ListMonoid *)
  type 'a t = 'a M.t
  let rec fold l = match l with [] -> M.zero | x :: xs -> M.plus x (fold xs)
  let rec mul n x = match n with 0 -> M.zero | _ -> M.plus x (mul (n - 1) x)
end


(* here it conforms to a functor type and returns a module *)
let (f: int -> int -> int) = fun (x : int) (y : int) -> x * y
module MonoidOperationsFunctor : MonoidOperationsFunctorType =
functor
  (M : Monoid)
  ->
  struct
    (* include ListMonoid *)
    type 'a t = 'a M.t
    let rec fold l = match l with [] -> M.zero | x :: xs -> M.plus x (fold xs)
    let rec mul n x = match n with 0 -> M.zero | _ -> M.plus x (mul (n - 1) x)
  end

module L = MonoidOperationsFunctor (ListMonoid)
module I = MonoidOperationsFunctor (IntMonoid)

(* of course such direct assignment between modules is allowed *)
module L2 = L

(* L, I have 2 functions: fold and mul *)
open L
open I

(* Example usage of L *)
let l_m = L.mul 2 [ 3; 4 ]

(* val lm : int list = [3; 4; 3; 4] *)
let l_f = L.fold [ [ 1; 2; 3 ]; [ 4; 5; 6 ] ]
(* val lf : int list = [1; 2; 3; 4; 5; 6] *)

(* Example usage of I *)
let i_m = I.mul 2 5

(* val i_m : int = 10 *)
let i_f = I.fold [ 1; 2; 4 ]
(* val i_f : int = 7 *)


(* The second way to define a functor: A direct functor definition, compared to the function def above 
Monoid is not the functor type it conforms to, but the return type of module,
therefore we have to provide 'a t, zero and plus. 

Note that the return type can be both module type or a functor type *)

(* Here it does not conform to anything, returns a module of type FoldAndMulModuleType *)
let f (x : int) (y : int) : int = x * y
module MonoidOperationsFunctor2 (M: Monoid): FoldAndMulModuleType with type 'a t = 'a M.t =  struct
  (* These 3 memebers are required by FoldAndMulModuleType *)
  type 'a t = 'a M.t
  let rec fold l = match l with [] -> M.zero | x :: xs -> M.plus x (fold xs)
  let rec mul n x = match n with 0 -> M.zero | _ -> M.plus x (mul (n - 1) x)
end

(* The equivalent definition as above *)
let f (x : int) (y : int) = x * y
module MonoidOperationsFunctor2 (M: Monoid) =  struct
  (* Don't need type 'a t = 'a M.t because we did not specify that 
     our return value is of type FoldAndMulModuleType *)
  let rec fold l = match l with [] -> M.zero | x :: xs -> M.plus x (fold xs)
  let rec mul n x = match n with 0 -> M.zero | _ -> M.plus x (mul (n - 1) x)
end

(* Of course a functor can take multiple arguments *)
module MonoidOperationsFunctor3 (M: Monoid) (N: Monoid) =  struct
  let rec foldn l = match l with [] -> N.zero | x :: xs -> N.plus x (foldn xs)
  let rec mulm n x = match n with 0 -> M.zero | _ -> M.plus x (mulm (n - 1) x)
end

(* of course applies also to the first kind of definition: *)
(* we don't conform MonoidOperationsType here because it only takes one argument *)
module MonoidOperationsFunctor3 = functor (M: Monoid) (N: Monoid) -> struct 
  let rec foldn l = match l with [] -> N.zero | x :: xs -> N.plus x (foldn xs)
  let rec mulm n x = match n with 0 -> M.zero | _ -> M.plus x (mulm (n - 1) x)

end 

(* Of course we can also create functors by conforming to a functor type 
   and make it equal to an existing implementation, which already conforms
   to that type *)
module MakeMonoidOperations : MonoidOperationsFunctorType = MonoidOperationsFunctor

(* how we use it *)
module FoldAndMulModule = MakeMonoidOperations (ListMonoid)


(* Of course a functor can also take functor as argument just like function can take function as argument *)
(* Here we use FuntorWithFunctorArg to demonstrate how arguments are handled *)
(* The first 2 defs are identical *)
let f (g : int -> int) = g 1
let f = fun (g : int -> int) -> g 1
module FuntorWithFunctorArg (M : MonoidOperationsFunctorType) = struct end
module FuntorWithFunctorArg = functor (M : MonoidOperationsFunctorType) -> struct end 

let f (g : int -> int) = fun () -> g 1
(* A functor that takes a functor and a unit as arguments, returns a struct  *)
module FuntorWithFunctorArg (M : MonoidOperationsFunctorType) = functor () -> struct end 

let f (g : int -> int) = fun (h : int -> int) -> g (h 1)
(* A functor that takes 2 functors as arguments, returns a struct  *)
module FuntorWithFunctorArg (M : MonoidOperationsFunctorType) = functor (M : MonoidOperationsFunctorType) -> struct end 

let f = fun () -> 1 + 1
(* A functor takes only a unit as argument and returns a struct *)
module FuntorWithFunctorArg = functor () -> struct end 

(* Notice: a functor type can only be implemented by a functor (implementation), not another functor type *)
(* module type F: MonoidOperationsFunctorType = functor (M: Monoid) -> sig ... end ===> cannot compile *)

(* Usage example of the functor *)
module Ops = MonoidOperationsFunctor (ListMonoid)

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
