let a (* : todo *) = (fun a b -> (+) b)
(* a on the right side can be safely removed because its not used *)
(* The operator + is underlied by a function called (+) which takes 2 int and returns the sum of them *)

let b (* : todo *) = (fun a b -> List.fold_left b 1 (List.map ( * ) a))
(* 
If a is [1;2;3], then (List.map ( * ) a) produces [(x) 1; (x) 2; (x)3]
b takes every func in that list and do something with acc and return the new acc. acc starts with 1
example b = fun acc x -> acc + (x acc)
let res = b [1;2;3] (fun acc x -> acc + (x acc))
val res: int = 24
*)

let c (* : todo *) = (fun a b c -> c (a + b)) 3
(* 
Breakdown
Anonymous Function Definition:
The code starts by defining an anonymous function using fun. The function has three parameters: a, b, and c.

fun a b c -> c (a + b)
This anonymous function takes three arguments:

a: an integer
b: an integer
c: a function that takes an integer and returns some result
The function adds a and b, and then applies the function c to the result of the addition.

Partial Application:
Next, the function is partially applied with the first argument 3.

(fun a b c -> c (a + b)) 3
This means that a is set to 3, resulting in a new function that takes the remaining two parameters, b and c.

After partial application, the function becomes:

fun b c -> c (3 + b)
Resulting Function x
After this partial application, x is now a function that takes two arguments: an integer b and a function c. The resulting function will add 3 to b and then apply c to the result.

In other words, the function x can be described as:

let x = fun b c -> c (3 + b)
*)

let d (* : todo *) = (fun a b c -> b (c a) :: [a]) "x"
(* 
d = fun b c -> b (c "a") :: ["a"]
It creates a list, which contains 2 elements: "a" and "a" after processed by c and then b
Similar to c
*)

let e (* : todo *) = (let x = List.map in x (<))
(* 
e takes one argument of list and returns a list of < applied to everyone element in the original list
Example: 
let res = e [1;2;3]
val: res = [(<) 1; (<) 2; (<) 3]
*)