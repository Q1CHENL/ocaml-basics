(* open the module Monoid. If the module name is not the same as the file name,
   e.g Module.ml and module Monoid, then should use "open Module" instead, even though
   open should be followed with a Module name. That is because ocaml then treats
   Moudle.ml as a module and Monoid as a submodule of Module.*)
open Monoid

(* First, what is a functor?
   > It's a higher order function which takes a module/functor and returns a module/functor.
   The term "higher-order" refers to functions that either:
   1. Take other functions as arguments, or
   2. Return functions as results.

   > In essence, it's really just like function, takes something, do some processing, returns
   > something. Just that here the arguments and the return values are all module/functors

   In the topic functor, there are 2 types of things:
   > functor type: the type of the functor
   > functor (implementation): the actual implementation of the functor
   They are just like module type and module (implementation), we can just view functor as a 
   function, and functor type as function type

   Comparison of function type and functor type:

   >        type ft = int                 -> int
   > module type FT = functor (M: Monoid) -> sig val f: 'a -> 'a ... end

   > This is only way to define a functor type.
   Note that after the name FT, no parameters are allowed, all parameters are defined 
   after the keyword functor.


   Comparison of function implementation and functor implementation:

   > 1. using keyword fun and functor, its actually a anynomous function/functor in the body
   >    let f: ft = fun      x          -> x * 2
   > module F: FT = functor (M: Monoid) -> struct let f x = x * 2 ... end
   
   > 2. direct implementation without keyword fun or functor
   >    let f (x:    int): int        = x * 2
    module F (M: Monoid): ListMonoid = struct ... end
   Here ": int" and ": ListMonoid" after the argument specifies the return type, not the 
   conforming type. 
   > Or more specifically, they represent the types of the expressions before them.
   Here, int is the type of "f (x: int)", ListMonoid is the type of "F (M: Monoid)", because in 
   ocaml, everything including function is a value, and has its own type.

   The argument of both functor and functor type is a module/functor type, not
   a module/functor (implementation). This is actually intutive, cuz of course the
   argument specified in the def of function/functor should a type, not an actual value.
*)

module type Monoid = sig
  type 'a t

  val zero : 'a t
  val plus : 'a t -> 'a t -> 'a t
end

(* A list module conforming to the module type Monoid *)
module ListMonoid : Monoid with type 'a t = 'a list = struct
  type 'a t = 'a list

  let zero = []
  let plus l1 l2 = l1 @ l2
end

(* A int module conforming to the module type Monoid *)
module IntMonoid : Monoid with type 'a t = int = struct
  type 'a t = int

  let zero = 0
  let plus x y = x + y
end

(* A module type, defines some functions *)
module type FoldAndMulModuleType = sig 
  type 'a t
  val fold : 'a t list -> 'a t
  val mul : int -> 'a t -> 'a t
end


(* A functor type 
   > It takes an argument of type Monoid, returns a module 
   It cannot be directly used to create modules, but rather
   is to be conformed to by a implementation of a functor 
   As said before, this is the only way to formally define a functor type except for direct assignment (see below)  *)
module type MonoidOperationsFunctorType = functor (M : Monoid) -> sig
  (* include Monoid *)

  val fold : 'a M.t list -> 'a M.t
  val mul : int -> 'a M.t -> 'a M.t
end


(* Or we can just make it return an existing module type 
   > Note that it should return a module type, not an actual module,
   > an actual module is returned in the implemenation of a functor    
*)
module type MonoidOperationsFunctorType = functor (M : Monoid) -> FoldAndMulModuleType with type 'a t = 'a M.t

(* We can use = to directly create new types with new names from the old ones *)
module type MonoidOperationsFunctorType2 = MonoidOperationsFunctorType


(* Compared to the similar function defination *)
(* Whenever this is argument right after the name of our functor, the thing after : is not the type 
   our functor conforms to, but the return type, which can be a module or a functor, it's similar to
   the function definition below, just that the right hand side fun x y -> x * y is not allowed in function 
   here it returns a functor of type MonoidOperationsFunctorType *)
(* let f (x : int) (y : int) : int = fun x y -> x * y *)
module FuntorWithFunctorArg (FT : MonoidOperationsFunctorType) = functor (FT : MonoidOperationsFunctorType) -> struct end 

(* A functor that conforms to the functor type above *)
(* It returns a module with 2 funtions: fold and mul.
   One misunderstanding may be: it must return a module that has something to do with the argument
   Well yes and no.
   Yes: it will use something defined in M, like a type, or function
   No: it could and often returns a group of completely new functions that are not in M at all

   Remember, functor is very flexible, it's really just like function, returns what we specify,
   not neccessarily includes the functions of the argument
*)
let f = fun x -> x + 1
let f: int -> int = fun x -> x + 1
(* Here it works also without ": MonoidOperationsFunctorType", just like we don't explicitly
   annotate the type of the function f with ": int -> int" *)
(* We also say that it conforms to the functor type MonoidOperationsFunctorType *)
(* Note that if we add a argument like (M: Monoid) after the functor name, then it becomes 
  a functor taking 2 arguments, just like we change f to f x *)
let f x = fun x -> x + 1
module MonoidOperationsFunctor : MonoidOperationsFunctorType = functor (M : Monoid) ->
  struct
  (* include ListMonoid *)
  type 'a t = 'a M.t
  let rec fold l = match l with [] -> M.zero | x :: xs -> M.plus x (fold xs)
  let rec mul n x = match n with 0 -> M.zero | _ -> M.plus x (mul (n - 1) x)
end


(* Usage of the functor *)
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


(* The second way to define a functor: A direct functor definition without using the keyword functor, 
   compared to the function def above Monoid is not the functor type it conforms to, but the return 
   type of module, or the type of MonoidOperationsFunctor2 (M: Monoid) as explained before, namely the
   type after apply to the argument M,　therefore we have to provide 'a t, zero and plus. 

  　Note that the return type can be both module type or a functor type *)
let f (x : int) (y : int) : int = x * y
module MonoidOperationsFunctor2 (M: Monoid): FoldAndMulModuleType with type 'a t = 'a M.t =  struct
  (* These 3 memebers are required by FoldAndMulModuleType *)
  type 'a t = 'a M.t
  let rec fold l = match l with [] -> M.zero | x :: xs -> M.plus x (fold xs)
  let rec mul n x = match n with 0 -> M.zero | _ -> M.plus x (mul (n - 1) x)
end

(* The equivalent definition as above, except that the return type is not annotated, which is allowed *)
let f (x : int) (y : int) = x * y
module MonoidOperationsFunctor2 (M: Monoid) = struct
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

(* Multiple arguments is valid for the first kind of definition: *)
(* we don't conform MonoidOperationsType here because it only takes one argument *)
module MonoidOperationsFunctor3 = functor (M: Monoid) (N: Monoid) -> struct 
  let rec foldn l = match l with [] -> N.zero | x :: xs -> N.plus x (foldn xs)
  let rec mulm n x = match n with 0 -> M.zero | _ -> M.plus x (mulm (n - 1) x)
end 

(* We can also create functors by conforming to a functor type and make it equal 
   to an existing implementation, which already conforms to that type *)
module MakeMonoidOperations : MonoidOperationsFunctorType = MonoidOperationsFunctor

(* how we use it *)
module FoldAndMulModule = MakeMonoidOperations (ListMonoid)


(* A functor can also take functor as argument just like function can take function as argument *)
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

(* Regarding the type annotation, the biggest difference between functor and function is that
   all the arguments of a functor must be annotated *)

(* Notice: a functor type can only be implemented by a functor (implementation), not another functor type (intuitive) *)
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
