(* OCaml has generic types *)
(* 'a and 'b etc are type variables. They can be instantiated by any type (but each
   occurrence with the same type), similar to T, K, V etc in Java *)
(* A comparison to Java:
   Java:  public class Stack<T> {...}

   OCaml:
   module Stack : sig =
      type 'a t
      ...
   end

   Correspondence:
   Java        OCaml
    T          'a
    Stack<T>   Stack/'a t  ===> here OCaml differentiate the name of the main type and the module name
    Stack      t

    In java, when we define a class Stack, we create a completely new type of the name Stack.
    Internally we might use a List/Array, but to outside, its a Stack object.

    In OCaml, we don't do module is not Object-Oriented Programming, but rather a group of relevant defs
    and functions. A module name is just a port to that group of stuff. It's easier to understand if we
    write type 'a stack instead of type 'a t

   And t is not a special keyword; it is simply a widely adopted convention. The
   convention of using t as a type name is used to refer to the main type defined
   by a module.
*)

type tree = Empty | Node of (int * int * int)

module type StackType = sig
  type 'a t

  val empty : unit -> 'a t
  val push : 'a -> 'a t -> 'a t
  val pop : 'a t -> 'a t
  val top : 'a t -> 'a option
end

module IntStack : StackType with type 'a t = int list = struct
  type 'a t = int list

  let empty () = []
  let push x stack = failwith ""
  let pop = failwith ""
  let top = failwith ""
end

module StackModule : StackType with type 'a t = 'a list = struct
  type 'a t = 'a list
  type 'b x = 'b list

  let empty () = []
  let push x s = x :: s
  let pop = function [] -> [] | _ :: s -> s
  let top = function [] -> None | x :: _ -> Some x
end

(* A functor, takes a unit, returns a module conforming to the module type Stack *)
module MakeStack () : StackType with type 'a t = 'a list = StackModule
module S = MakeStack ()

let s = S.push 1 []

module MakeStack () : StackType = struct
  type 'a t = 'a list

  let empty () = []
  let push x s = x :: s
  let pop = function [] -> [] | _ :: s -> s
  let top = function [] -> None | x :: _ -> Some x
end

module S = MakeStack ()

(* Does not compile without "with type 'a t = 'a list"  *)
let s = S.push 1 []
let stack = StackModule.empty
let istack = StackModule.push 1 []
let sstack = StackModule.push "" []
