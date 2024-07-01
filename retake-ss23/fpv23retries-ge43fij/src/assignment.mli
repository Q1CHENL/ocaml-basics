module type Trie = sig
  (* (<is_word>, <children_list>) *)
  type trie = Trie of (bool * (char * trie) list)

  val empty : trie
  val insert : trie -> char list -> trie
  val remove : trie -> char list -> trie
  val contains : trie -> char list -> bool
end

module type Trie_db = functor (Trie : Trie) -> sig
  type t

  val create : unit -> t
  val insert : t -> char list -> unit
  val remove : t -> char list -> unit
  val contains : t -> char list -> bool
end

module Trie : Trie
module Trie_db : Trie_db

module type Helpers = sig
  val string_to_char_list : string -> char list
  val char_list_to_string : char list -> string
end

module Helpers : Helpers

val example_trie_direct : unit -> Trie.trie
val example_trie_insert : unit -> Trie.trie
val example_trie_insert_db : unit -> Trie_db(Trie).t
