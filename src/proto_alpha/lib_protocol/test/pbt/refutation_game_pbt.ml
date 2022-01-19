(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(* Permission is hereby granted, free of charge, to any person obtaining a   *)
(* copy of this software and associated documentation files (the "Software"),*)
(* to deal in the Software without restriction, including without limitation *)
(* the rights to use, copy, modify, merge, publish, distribute, sublicense,  *)
(* and/or sell copies of the Software, and to permit persons to whom the     *)
(* Software is furnished to do so, subject to the following conditions:      *)
(*                                                                           *)
(* The above copyright notice and this permission notice shall be included   *)
(* in all copies or substantial portions of the Software.                    *)
(*                                                                           *)
(* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR*)
(* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,  *)
(* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL   *)
(* THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER*)
(* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING   *)
(* FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER       *)
(* DEALINGS IN THE SOFTWARE.                                                 *)
(*                                                                           *)
(*****************************************************************************)

(** Testing
    -------
    Component:    Protocol Library
    Invocation:   dune exec \
                  src/proto_alpha/lib_protocol/test/pbt/refutation_game_pbt.exe
    Subject:      SCORU refutation game
*)

open Protocol.Refutation_game

exception TickNotFound of Tick.t

open Lib_test.Qcheck_helpers

(**
Helpers
*)
let option_get = function
  | Some a -> a
  | None -> raise (Invalid_argument "option is None")

module MakeCountingPVM (P : sig
  val target : int
end) : sig
  include PVM with type _ state = int

  val target : int
end = struct
  let target = P.target

  type _ state = int

  let compress x = x

  let initial_state = 0

  let random_state _ _ = Random.bits ()

  let string_of_state = string_of_int

  let equal_state = ( = )

  type tick = Tick.t

  type history = {states : int Tick.Map.t; tick : tick}

  let remember history tick state =
    {history with states = Tick.Map.add tick state history.states}

  exception TickNotFound of tick

  let eval ~failures (tick : Tick.t) state =
    if state >= P.target then state
    else if List.mem ~equal:Tick.( = ) tick failures then state
    else state + 1

  let execute_until ~failures tick state pred =
    let rec loop state tick =
      if pred tick state then (tick, state)
      else
        let state = eval ~failures tick state in
        loop state (Tick.next tick)
    in
    loop state tick

  let state_at history tick =
    let (lower, ostate, _) = Tick.Map.split tick history.states in
    match ostate with
    | Some state -> state
    | None ->
        let (tick0, state0) =
          match Tick.Map.max_binding lower with
          | Some s -> s
          | None -> (Tick.make 0, 0)
        in
        snd
          (execute_until ~failures:[] tick0 state0 (fun tick' _ -> tick' = tick))

  let verifiable_state_at = state_at

  let empty_history = {states = Tick.Map.empty; tick = Tick.make 0}
end

let operation state number =
  Digest.bytes @@ Bytes.of_string @@ state ^ string_of_int number

module RandomPVM (P : sig
  val initial_prog : int list
end) : sig
  include PVM with type _ state = string * int list
end = struct
  exception TickNotFound of Tick.t

  type _ state = string * int list

  let compress x = x

  let initial_state = ("hello", P.initial_prog)

  let random_state length (_, program) =
    let remaining_program = TzList.drop_n length program in
    let stop_state = (operation "" (Random.bits ()), remaining_program) in
    stop_state

  let string_of_state (st, li) =
    st ^ List.fold_left (fun acc x -> acc ^ ";" ^ string_of_int x) "" li

  let equal_state = ( = )

  type history = {states : (string * int list) Tick.Map.t; tick : Tick.t}

  let remember history tick state =
    {history with states = Tick.Map.add tick state history.states}

  let eval ~failures (tick : Tick.t) ((hash, continuation) as state) =
    match continuation with
    | [] -> state
    | h :: tl ->
        if List.mem ~equal:( = ) tick failures then (hash, tl)
        else (operation hash h, tl)

  let execute_until ~failures tick state pred =
    let rec loop state tick =
      if pred tick state || snd state = [] then (tick, state)
      else
        let state = eval ~failures tick state in
        loop state (Tick.next tick)
    in
    loop state tick

  let state_at history tick =
    let (lower, ostate, _) = Tick.Map.split tick history.states in
    match ostate with
    | Some state -> state
    | None ->
        let (tick0, state0) =
          match Tick.Map.max_binding lower with
          | Some (t, s) -> (t, s)
          | None -> raise Not_found
        in
        snd
          (execute_until ~failures:[] tick0 state0 (fun tick' _ -> tick' = tick))

  let verifiable_state_at = state_at

  let empty_history = {states = Tick.Map.empty; tick = Tick.make 0}

  type tick = Tick.t
end

module MerkelizedMichelson = struct
  (**

     We assume the existence of some cryptographic hashing
     function. Notice that such function is relatively slow.
     Therefore, the interpreter should compute hashes only when
     strictly required.

   *)
  module Hash : sig
    type t

    (** This function will more likely be of type [Bytes.t -> t]. *)
    val hash : 'a -> t

    (** [combine hs] returns a hash for hash list [hs].  *)
    val combine : t list -> t

    (** The size in bytes of the serialization of an hash. *)
    val size : int

    val to_string : t -> string
  end = struct
    type t = Digest.t

    let hash x = Digest.bytes (Marshal.to_bytes x [])

    let to_string x = String.sub (Digest.to_hex x) 0 8

    let rec combine = function
      | [] -> assert false
      | [h] -> h
      | h :: hs -> hash (h, combine hs)

    let size = 16
  end

  (**

     We need a notion of [Taint] to mark specific parts of values to
     understand if they have been useful for a given execution step.

  *)
  module Taint : sig
    type t

    val transparent : t

    val of_tick : Tick.t -> t

    val fresh : unit -> t

    val to_string : t -> string
  end = struct
    type t = int

    let transparent = -1

    let fresh =
      let r = ref (-1) in
      fun () ->
        decr r ;
        !r

    let of_tick (t : Tick.t) = (t :> int)

    let to_string = string_of_int
  end

  (**

     The following type constructor is used at each level of the
     values manipulated by the merkelizing interpreter.

     Hopefully, the introduction of such a type constructor will be
     the major change in the existing Michelson interpreter.

     Indeed, to recover the standard Michelson interpreter, one can
     simply use the type constructor ['a id = 'a] instead of ['a
     merkelized].

  *)
  type 'a merkelized = {
    value : 'a option;  (** A merkelized value can be absent. *)
    repr : 'a repr;  (** A dynamic representation of ['a]'s type. *)
    mutable hash : Hash.t option;
        (** A value of type ['a merkelized] is actually "premerkelized"
          as the hashes are only computed on demand by a (recursive)
          modification of the [hash] field. *)
    mutable mark : Taint.t;
        (** A [mark] represents a taintstamp with respect to the
         execution witnessing that the value has been observed at some
         point of the execution.

         We must maintain the invariant that the mark of subvalues are
         older than [mark]. *)
    size : int;
        (**

          We need to maintain the size of merkelized values because we
          must be able to transmit them in a layer-1 operation (which
          has a limited size).

          What we call "size" in this context is not the size of the
          complete merkelized value but simply the size of the serialization
          of the toplevel structure of the merkelized value.

          Typically, for an inhabitant of an algebraic data type,
          this size is typically the size of the data constructor plus
          the size of the hashes of its components.

          In the real implementation, we will probably need to transmit
          serialized values instead of hashes for small values.

        *)
  }

  and 'a v = 'a merkelized

  and ('a, 's) cell = 'a v * 's v

  and ('s, 'f) cont =
    | KHalt : ('f, 'f) cont
    | KCons : ('s, 't) instr v * ('t, 'f) cont v -> ('s, 'f) cont

  (**

      A small subset of Michelson.

  *)
  and ('s, 'u) instr =
    | Halt : ('f, 'f) instr
    | Push : 'a v * (('a, 's) cell, 'f) instr v -> ('s, 'f) instr
    | Succ : ((int, 's) cell, 'u) instr v -> ((int, 's) cell, 'u) instr
    | Mul :
        ((int, 's) cell, 'f) instr v
        -> ((int, (int, 's) cell) cell, 'f) instr
    | Dec : ((int, 's) cell, 'f) instr v -> ((int, 's) cell, 'f) instr
    | CmpNZ : ((bool, 's) cell, 'f) instr v -> ((int, 's) cell, 'f) instr
    | Loop :
        ('s, (bool, 's) cell) instr v * ('s, 'f) instr v
        -> ((bool, 's) cell, 'f) instr
    | Dup : (('a, ('a, 's) cell) cell, 'f) instr v -> (('a, 's) cell, 'f) instr
    | Swap :
        (('b, ('a, 's) cell) cell, 'f) instr v
        -> (('a, ('b, 's) cell) cell, 'f) instr
    | Drop : ('s, 'f) instr v -> (('a, 's) cell, 'f) instr
    | Dip :
        ('s, 't) instr v * (('a, 't) cell, 'f) instr v
        -> (('a, 's) cell, 'f) instr

  and _ repr =
    | Int : int repr
    | Bool : bool repr
    | Unit : unit repr
    | Cell : 'a repr * 'b repr -> ('a, 'b) cell repr
    | Instr : 'i repr * 'o repr -> ('i, 'o) instr repr
    | Cont : 'a repr * 'b repr -> ('a, 'b) cont repr

  (** To simplify testing, we assume that a program has no argument
      and always produces an integer. *)
  type program = (unit, (int, unit) cell) instr v

  let hash_of v = option_get v.hash

  let rec string_of_repr : type a. a repr -> string = function
    | Cell (ity, oty) ->
        Printf.sprintf "(%s, %s)" (string_of_repr ity) (string_of_repr oty)
    | Cont (ity, oty) ->
        Printf.sprintf "(%s ~> %s)" (string_of_repr ity) (string_of_repr oty)
    | Instr _ -> "instr"
    | Int -> "int"
    | Bool -> "bool"
    | Unit -> "unit"

  let rec string_of_value : type a. a repr -> a -> string =
   fun repr x ->
    match repr with
    | Cell (_ity, _oty) ->
        Printf.sprintf "(%s, %s)" (show (fst x)) (show (snd x))
    | Cont _ -> string_of_cont x
    | Instr _ -> string_of_instr x
    | Int -> string_of_int x
    | Bool -> string_of_bool x
    | Unit -> "()"

  and string_of_instr : type a s. (a, s) instr -> string = function
    | Halt -> "halt"
    | Push (x, i) -> Printf.sprintf "push %s ; %s" (show x) (show i)
    | Succ i -> Printf.sprintf "succ ; %s" (show i)
    | Mul i -> Printf.sprintf "mul ; %s" (show i)
    | Dec i -> Printf.sprintf "dec ; %s" (show i)
    | CmpNZ i -> Printf.sprintf "cmpnz ; %s" (show i)
    | Loop (body, the_exit) ->
        Printf.sprintf "loop { %s } ; %s" (show body) (show the_exit)
    | Dup i -> Printf.sprintf "dup ; %s" (show i)
    | Swap i -> Printf.sprintf "swap ; %s" (show i)
    | Drop i -> Printf.sprintf "drop ; %s" (show i)
    | Dip (body, and_then) ->
        Printf.sprintf "dip { %s } ; %s" (show body) (show and_then)

  and string_of_cont : type s f. (s, f) cont -> string = function
    | KHalt -> "khalt"
    | KCons (i, k) -> Printf.sprintf "%s : %s" (show i) (show k)

  and verbose = false

  and show : type a. a v -> string =
   fun v ->
    if verbose then
      Printf.sprintf
        "([%s | %s ]%s : %s)"
        (Taint.to_string v.mark)
        (Option.fold ~none:"?" ~some:Hash.to_string v.hash)
        (Option.fold ~none:"" ~some:(string_of_value v.repr) v.value)
        (string_of_repr v.repr)
    else
      Printf.sprintf
        "([%s]%s)"
        (Option.fold ~none:"?" ~some:Hash.to_string v.hash)
        (Option.fold ~none:"" ~some:(string_of_value v.repr) v.value)

  let get ~taint left_space v =
    if taint <> Taint.transparent then v.mark <- taint ;
    left_space := !left_space - v.size ;
    if !left_space < 0 then
      raise (Invalid_argument "This operation consumes too much space") ;
    option_get v.value

  let merke ~taint repr value size =
    {hash = None; value = Some value; repr; size; mark = taint}

  let push ~taint x i =
    match i.repr with
    | Instr (Cell (_, s), f) ->
        merke ~taint (Instr (s, f)) (Push (x, i)) (24 + (2 * Hash.size))

  let halt ~taint f = merke ~taint (Instr (f, f)) Halt (8 + Hash.size)

  let succ ~taint i = merke ~taint i.repr (Succ i) (24 + Hash.size)

  let mul ~taint i =
    match i.repr with
    | Instr (s, f) ->
        merke ~taint (Instr (Cell (Int, s), f)) (Mul i) (24 + Hash.size)

  let dec ~taint i = merke ~taint i.repr (Dec i) (24 + i.size)

  let cmpnz ~taint i =
    match i.repr with
    | Instr (Cell (_, s), f) ->
        merke ~taint (Instr (Cell (Int, s), f)) (CmpNZ i) (24 + Hash.size)

  let dup ~taint i =
    match i.repr with
    | Instr (Cell (_, Cell (a, s)), f) ->
        merke ~taint (Instr (Cell (a, s), f)) (Dup i) (24 + Hash.size)

  let swap ~taint i =
    match i.repr with
    | Instr (Cell (b, Cell (a, s)), f) ->
        merke ~taint (Instr (Cell (a, Cell (b, s)), f)) (Swap i) (24 + Hash.size)

  let drop ~taint ty i =
    match i.repr with
    | Instr (s, f) ->
        merke ~taint (Instr (Cell (ty, s), f)) (Drop i) (24 + Hash.size)

  let loop ~taint body the_exit =
    match (body.repr, the_exit.repr) with
    | (Instr (_, i), Instr (_, f)) ->
        merke
          ~taint
          (Instr (i, f))
          (Loop (body, the_exit))
          (24 + (2 * Hash.size))

  let dip ~taint body and_then =
    match (and_then.repr, body.repr) with
    | (Instr (Cell (a, _), f), Instr (s', _)) ->
        merke
          ~taint
          (Instr (Cell (a, s'), f))
          (Dip (body, and_then))
          (24 + (2 * Hash.size))

  let lint ~taint x = merke ~taint Int x 8

  let lbool ~taint x = merke ~taint Bool x 8

  let khalt ~taint f = merke ~taint (Cont (f, f)) KHalt 8

  let kcons ~taint (type s t f) (i : (s, t) instr v) (cont : (t, f) cont v) :
      (s, f) cont v =
    match i.repr with
    | Instr (ity, _) -> (
        match cont.repr with
        | Cont (_, oty) ->
            merke
              ~taint
              (Cont (ity, oty))
              (KCons (i, cont))
              (i.size + cont.size + 24))

  let cell ~taint x v =
    merke ~taint (Cell (x.repr, v.repr)) (x, v) (x.size + v.size + 24)

  let empty_stack ~taint = merke ~taint Unit () 8

  (** [merkelize v] computes the hashes in [v]. *)
  let rec merkelize : type a. a v -> unit =
    let open Hash in
    fun v ->
      match v.hash with
      | Some _ -> ()
      | None ->
          let hash =
            match (v.repr, v.value) with
            | (Cell _, Some x) ->
                let (a, b) = x in
                merkelize a ;
                merkelize b ;
                assert (v.hash = None) ;
                (* No recursive values allowed. *)
                combine [hash_of a; hash_of b]
            | (Instr _, Some i) -> hash_instr i
            | (Cont _, Some k) -> hash_cont k
            | (Int, Some i) -> hash i
            | (Bool, Some i) -> hash i
            | (Unit, Some ()) -> hash ()
            | (_, None) -> assert false
          in
          v.hash <- Some hash

  and hash_cont : type s f. (s, f) cont -> Hash.t = function
    | KHalt -> Hash.hash "KHalt"
    | KCons (i, cont) ->
        merkelize i ;
        merkelize cont ;
        Hash.(combine [hash "KCons"; hash_of i; hash_of cont])

  and hash_instr : type s f. (s, f) instr -> Hash.t = function
    | Halt -> Hash.hash ["Halt"]
    | Push (x, i) ->
        merkelize x ;
        merkelize i ;
        Hash.(combine [hash "Push"; hash_of x; hash_of i])
    | Succ i ->
        merkelize i ;
        Hash.(combine [hash "Succ"; hash_of i])
    | Mul i ->
        merkelize i ;
        Hash.(combine [hash "Mul"; hash_of i])
    | Dec i ->
        merkelize i ;
        Hash.(combine [hash "Dec"; hash_of i])
    | CmpNZ i ->
        merkelize i ;
        Hash.(combine [hash "CmpNZ"; hash_of i])
    | Loop (body, the_exit) ->
        merkelize body ;
        merkelize the_exit ;
        Hash.(combine [hash "Loop"; hash_of body; hash_of the_exit])
    | Dup i ->
        merkelize i ;
        Hash.(combine [hash "Dup"; hash_of i])
    | Swap i ->
        merkelize i ;
        Hash.(combine [hash "Swap"; hash_of i])
    | Drop i ->
        merkelize i ;
        Hash.(combine [hash "Drop"; hash_of i])
    | Dip (body, and_then) ->
        merkelize body ;
        merkelize and_then ;
        Hash.(combine [hash "Dip"; hash_of body; hash_of and_then])

  (** [useful_part ~taint v] returns the (toplevel) parts of [v]
      that have been marked with [taint].

      We assume that the root of [v] has been marked with [taint].
      Otherwise, the result is undefined.
   *)
  let useful_part ~taint v =
    let rec aux : type a. a v -> a v =
     fun v ->
      if v.mark <> taint then {v with value = None}
      else
        match (v.repr, v.value) with
        | (Cell _, Some x) -> {v with value = Some (aux (fst x), aux (snd x))}
        | (Cont _, Some (KCons (i, k))) ->
            {v with value = Some (KCons (aux i, aux k))}
        | (Cont _, Some KHalt) -> v
        | (Instr _, Some i) ->
            let value =
              match i with
              | Push (x, i) -> Push (aux x, aux i)
              | Halt -> Halt
              | Succ i -> Succ (aux i)
              | Mul i -> Mul (aux i)
              | Dec i -> Dec (aux i)
              | CmpNZ i -> CmpNZ (aux i)
              | Loop (body, the_exit) -> Loop (aux body, aux the_exit)
              | Dup i -> Dup (aux i)
              | Swap i -> Swap (aux i)
              | Drop i -> Drop (aux i)
              | Dip (body, and_then) -> Dip (aux body, aux and_then)
            in
            {v with value = Some value}
        | (Int, _) -> v
        | (Bool, _) -> v
        | (Unit, _) -> v
        | (_, None) -> v
    in
    aux v
end

module MPVM (Code : sig
  val program : MerkelizedMichelson.program
end) =
struct
  open MerkelizedMichelson

  (**

      The state of the Merkelized interpreter for Michelson is
      made of merkelized pair of a stack of type ['s] and a
      continuation expecting a stack of this type.

      ['f] represents the type of the final stack.

      Both ['s] and ['f] are existentially quantified but can
      be recovered thanks to their dynamic representation held
      by the [repr] field of the merkelized value.

  *)
  type _ state = State : ('s, ('s, 'f) cont) cell v -> _ state

  let string_of_state (State cell) = show cell

  let merkelize_state (State s) =
    merkelize s ;
    State s

  (** This function "compresses" the state representation by dropping the
      value. This should reduce the size of the serialization. However, if
      the value is shorter than the hash, we should probably keep the value
      and recompute the hash on the other side of the pipe. *)
  let compress (State s) = State {s with value = None}

  let initial_state =
    let taint = Taint.transparent in
    let stack = empty_stack ~taint in
    let cont = kcons ~taint Code.program (khalt ~taint (Cell (Int, Unit))) in
    merkelize_state (State (cell ~taint stack cont))

  let random_state _ _ =
    let taint = Taint.transparent in
    let s = cell ~taint (empty_stack ~taint) (khalt ~taint Unit) in
    compress (merkelize_state (State s))

  (**

     [step_instr ~taint i k stack] implements a single execution step.

     This interpreter is close the current Michelson interpreter
     except on the following points:

     - There is no recursive call to immediately evaluate the next
       instruction.

     - Values are deconstructed through [get] which taints them to
       determine their useful parts. As said earlier, replacing
       [get] with the identity should recover the current Michelson
       interpreter.

     - This interpreter checks that the useful part of the state
       remains compatible with the size limit of its serialization.

  *)
  let step_instr :
      type s t f.
      taint:Taint.t ->
      int ref ->
      (s, t) instr v ->
      (t, f) cont v ->
      s v ->
      [`Verifiable | `Full] state =
   fun ~taint left_space i cont s ->
    let return s = State s in
    match get ~taint left_space i with
    | Halt -> return (cell ~taint s cont)
    | Push (x, i) ->
        return (cell ~taint (cell ~taint x s) (kcons ~taint i cont))
    | Succ i -> (
        match get ~taint left_space s with
        | (x, s) ->
            let x = get ~taint left_space x in
            return
              (cell
                 ~taint
                 (cell ~taint (lint ~taint (x + 1)) s)
                 (kcons ~taint i cont)))
    | Mul i -> (
        match get ~taint left_space s with
        | (x, s) -> (
            match get ~taint left_space s with
            | (y, s) ->
                let x = get ~taint left_space x in
                let y = get ~taint left_space y in
                return
                  (cell
                     ~taint
                     (cell ~taint (lint ~taint (x * y)) s)
                     (kcons ~taint i cont))))
    | Dec i -> (
        match get ~taint left_space s with
        | (x, s) ->
            let x = get ~taint left_space x in
            return
              (cell
                 ~taint
                 (cell ~taint (lint ~taint (x - 1)) s)
                 (kcons ~taint i cont)))
    | CmpNZ i -> (
        match get ~taint left_space s with
        | (x, s) ->
            let x = get ~taint left_space x in
            return
              (cell
                 ~taint
                 (cell ~taint (lbool ~taint (x <> 0)) s)
                 (kcons ~taint i cont)))
    | Loop (body, the_exit) -> (
        match get ~taint left_space s with
        | (b, s) ->
            if get ~taint left_space b then
              return (cell ~taint s (kcons ~taint body (kcons ~taint i cont)))
            else return (cell ~taint s (kcons ~taint the_exit cont)))
    | Dup i -> (
        match get ~taint left_space s with
        | (x, _) -> return (cell ~taint (cell ~taint x s) (kcons ~taint i cont))
        )
    | Swap i -> (
        match get ~taint left_space s with
        | (x, s) -> (
            match get ~taint left_space s with
            | (y, s) ->
                return
                  (cell
                     ~taint
                     (cell ~taint y (cell ~taint x s))
                     (kcons ~taint i cont))))
    | Drop i -> (
        match get ~taint left_space s with
        | (_, s) -> return (cell ~taint s (kcons ~taint i cont)))
    | Dip (body, and_then) -> (
        match get ~taint left_space s with
        | (x, s) ->
            let cont =
              kcons ~taint body (kcons ~taint (push ~taint x and_then) cont)
            in
            return (cell ~taint s cont))

  let step_cont :
      type s f.
      taint:Taint.t ->
      int ref ->
      (s, f) cont v ->
      s v ->
      [`Verifiable | `Full] state =
   fun ~taint left_space cont s ->
    match get ~taint left_space cont with
    | KHalt -> State (cell ~taint s cont)
    | KCons (i, cont) -> step_instr ~taint left_space i cont s

  let equal_state : _ state -> _ state -> bool =
   fun (State s1) (State s2) ->
    match (s1.hash, s2.hash) with
    | (Some h1, Some h2) -> h1 = h2
    | (_, _) -> assert false

  (**

     The history contains at most K snapshots of the state
     where K is the number of section in the previous dissection.

     Hence, if the initial section of the game has N ticks, at
     the step I, we have (N / K^I) execution steps to replay.
     This looks reasonable.

  *)

  type history = [`Verifiable | `Full] state Tick.Map.t

  let empty_history : history = Tick.Map.empty

  let remember history (tick : Tick.t) state = Tick.Map.add tick state history

  type tick = Tick.t

  exception TickNotFound of tick

  let forward_eval history tick =
    match Tick.Map.split tick history with
    | (lower, None, _) -> Tick.Map.max_binding lower
    | (_, Some state, _) -> Some (tick, state)

  let eval_to : taint:Taint.t -> history -> tick -> [`Verifiable | `Full] state
      =
   fun ~taint history target_tick ->
    let (tick0, state0) = option_get @@ forward_eval history target_tick in
    let rec go tick state =
      if tick = target_tick then state
      else
        let (State s) = state in
        let left_space = ref (16 * 1024) in
        let (v, cont) = get ~taint left_space s in
        let state' = step_cont ~taint left_space cont v in
        go (Tick.next tick) state'
    in
    go tick0 state0

  let state_at : history -> tick -> [`Verifiable | `Full] state =
   fun history tick ->
    let taint = Taint.of_tick tick in
    merkelize_state @@ eval_to ~taint history tick

  let verifiable_state_at : history -> tick -> [`Verifiable] state =
   fun history tick ->
    let (State s0) = state_at history tick in
    let taint = Taint.of_tick tick in
    let left_space = ref (16 * 1024) in
    let (v, cont) = get ~taint left_space s0 in
    let _ = step_cont ~taint left_space cont v in
    State (useful_part ~taint s0)

  let eval :
      failures:tick list -> tick -> ([> `Verifiable] as 'a) state -> 'a state =
   fun ~failures tick (State s) ->
    if List.mem ~equal:Tick.( = ) tick failures then
      (* In this case, a failure is a stuttering. *)
      State s
    else
      let taint = Taint.of_tick tick in
      let left_space = ref (16 * 1024) in
      let (v, cont) = get ~taint left_space s in
      merkelize_state (step_cont ~taint left_space cont v)

  let rec execute_until :
      failures:tick list ->
      tick ->
      ([> `Verifiable] as 'a) state ->
      (tick -> 'a state -> bool) ->
      tick * 'a state =
   fun ~failures tick state pred ->
    if pred tick state then (tick, merkelize_state state)
    else
      let state = eval ~failures tick state in
      execute_until ~failures (Tick.next tick) state pred
end

module Push = struct
  open MerkelizedMichelson

  let program =
    let taint = Taint.transparent in
    push ~taint (lint ~taint 1) (halt ~taint (Cell (Int, Unit)))
end

module Fact20 = struct
  open MerkelizedMichelson

  let program : program =
    let taint = Taint.transparent in

    push ~taint (lint ~taint 20)
    @@ push ~taint (lint ~taint 1)
    @@ dup ~taint @@ cmpnz ~taint
    @@ dip ~taint (swap ~taint (halt ~taint (Cell (Int, Cell (Int, Unit)))))
    @@ loop ~taint
         (dup ~taint
         @@ dip ~taint
              (swap ~taint (halt ~taint (Cell (Int, Cell (Int, Unit)))))
         @@ mul ~taint @@ swap ~taint @@ dec ~taint @@ dup ~taint
         @@ cmpnz ~taint
              (halt ~taint (Cell (Bool, Cell (Int, Cell (Int, Unit))))))
    @@ drop ~taint Int (halt ~taint (Cell (Int, Unit)))
end

module Strategies (G : Game) = struct
  open G
  open PVM

  let random_tick ?(from = 0) () = Tick.make (from + Random.int 31)

  let random_section (start_at : Tick.t) start_state (stop_at : Tick.t) =
    let x = min 10000 (abs (Tick.distance start_at stop_at)) in
    let length = 1 + try Random.int x with _ -> 0 in
    let stop_at = (start_at :> int) + length in

    ({
       section_start_at = start_at;
       section_start_state = start_state;
       section_stop_at = Tick.make stop_at;
       section_stop_state = compress @@ random_state length start_state;
     }
      : _ G.section)

  let random_dissection (gsection : [`Compressed] G.section) =
    let rec aux dissection start_at start_state =
      if start_at = gsection.section_stop_at then dissection
      else
        let section =
          random_section start_at start_state gsection.section_stop_at
        in
        if
          section.section_start_at = gsection.section_start_at
          && section.section_stop_at = gsection.section_stop_at
        then aux dissection start_at start_state
        else
          aux
            (section :: dissection)
            section.section_stop_at
            section.section_stop_state
    in
    if Tick.distance gsection.section_stop_at gsection.section_start_at > 1 then
      Some
        (aux [] gsection.section_start_at gsection.section_start_state
        |> List.rev)
    else None

  let compress_section section =
    {
      section with
      section_start_state = compress section.section_start_state;
      section_stop_state = compress section.section_stop_state;
    }
  (* let {section_start_state; section_start_at; section_stop_state; section_stop_at} = section in
     {(compress section_start_state); section_start_at; (compress section_stop_state); section_stop_at} *)

  let random_decision d =
    let x = Random.int (List.length d) in
    let section =
      match List.(nth d x) with Some s -> s | None -> raise Not_found
    in
    let section_start_at = section.section_start_at in
    let section_stop_at = section.section_stop_at in
    let section_start_state = random_state 0 section.section_start_state in
    let section_stop_state =
      random_state
        ((section_stop_at :> int) - (section_start_at :> int))
        section.section_start_state
    in
    let next_dissection = random_dissection section in
    let section =
      {
        section_start_state;
        section_start_at;
        section_stop_state;
        section_stop_at;
      }
    in
    let conflict_search_step =
      match next_dissection with
      | None ->
          G.Conclude
            {
              start_state = section.section_start_state;
              stop_state = compress section.section_stop_state;
            }
      | Some next_dissection ->
          G.Refine
            {stop_state = compress section.section_stop_state; next_dissection}
    in
    G.ConflictInside {choice = compress_section section; conflict_search_step}

  type parameters = {
    branching : int;
    failing_level : int;
    max_failure : int option;
  }

  type checkpoint = Tick.t -> bool

  type strategy = Random | MachineDirected of parameters * checkpoint

  let conflicting_section (history : PVM.history) (section : _ G.section) =
    not
      (equal_state
         section.section_stop_state
         (state_at history section.section_stop_at))

  (** corrected, optimised and inlined version of the split (only one pass of the list rather than 3)*)
  let dissection_from_section history branching (section : _ G.section) =
    if Tick.next section.section_start_at = section.section_stop_at then None
    else
      let start = (section.section_start_at :> int) in
      let stop = (section.section_stop_at :> int) in
      let len = stop - start in
      let bucket = len / branching in
      let dissection =
        init branching (fun x ->
            let start_at = start + (bucket * x) in
            let stop_at =
              if x = branching - 1 then stop
              else min stop (start + (bucket * (x + 1)))
            in
            let section_start_at = Tick.make start_at
            and section_stop_at = Tick.make stop_at in
            ({
               section_start_at;
               section_start_state = PVM.state_at history section_start_at;
               section_stop_at;
               section_stop_state = PVM.state_at history section_stop_at;
             }
              : _ G.section))
      in
      Some dissection

  let compress_section (section : _ G.section) : [`Compressed] G.section =
    {
      section with
      section_start_state = PVM.compress section.section_start_state;
      section_stop_state = PVM.compress section.section_stop_state;
    }

  let remember_section history (section : [`Verifiable | `Full] G.section) =
    let history =
      PVM.remember history section.section_start_at section.section_start_state
    in
    PVM.remember history section.section_stop_at section.section_stop_state

  let next_move history branching dissection =
    let section =
      List.find_opt (conflicting_section history) dissection |> function
      | None -> raise (TickNotFound (Tick.make 0))
      | Some s -> s
    in
    let next_dissection = dissection_from_section history branching section in
    let (conflict_search_step, history) =
      match next_dissection with
      | None ->
          let stop_state =
            state_at history (Tick.next section.section_start_at)
          in
          let stop_state = PVM.(compress stop_state) in
          ( G.Conclude
              {
                start_state =
                  PVM.(verifiable_state_at history section.section_start_at);
                stop_state;
              },
            empty_history )
      | Some next_dissection ->
          let stop_state =
            PVM.(compress (state_at history section.section_stop_at))
          in
          let history =
            List.fold_left remember_section empty_history next_dissection
          in
          let next_dissection = List.map compress_section next_dissection in
          (G.Refine {stop_state; next_dissection}, history)
    in
    (G.ConflictInside {choice = section; conflict_search_step}, history)

  let generate_failures failing_level (section_start_at : Tick.t)
      (section_stop_at : Tick.t) max_failure =
    let d = Tick.distance section_stop_at section_stop_at in
    let d =
        match max_failure with
        | None -> d
        | Some x -> max x 1
      in
      if failing_level > 0 then
        let s =
          init failing_level (fun _ ->
              let s = (section_start_at :> int) + Random.int (max d 1) in
              Tick.make s)
        in
        s
      else []

  let machine_directed_committer {branching; failing_level; max_failure} pred =
    let history = ref PVM.empty_history in
    let initial ((section_start_at : Tick.t), section_start_state) : G.commit =
      let section_stop_at =
        Tick.make ((section_start_at :> int) + Random.int 2)
      in
      let failures =
        generate_failures
          failing_level
          section_start_at
          section_stop_at
          max_failure
      in
      let (section_stop_at, section_stop_state) =
        PVM.execute_until ~failures section_start_at section_start_state
        @@ fun tick _ -> pred tick
      in
      history := PVM.remember !history section_start_at section_start_state ;
      history := PVM.remember !history section_stop_at section_stop_state ;
      let section_start_state = PVM.compress section_start_state in
      let section_stop_state = PVM.compress section_stop_state in
      Commit
        {
          section_start_state;
          section_start_at;
          section_stop_state;
          section_stop_at;
        }
    in
    let next_move dissection =
      let (move, history') = next_move !history branching dissection in
      history := history' ;
      move
    in
    ({initial; next_move} : _ G.client)

  let machine_directed_refuter {branching; failing_level; max_failure} =
    let history = ref PVM.empty_history in
    let initial (section_start_state, Commit section) : G.refutation =
      let ({section_start_at; section_stop_at; _} : _ G.section) = section in
      let failures =
        generate_failures
          failing_level
          section_start_at
          section_stop_at
          max_failure
      in
      let (_stop_at, section_stop_state) =
        PVM.execute_until ~failures section_start_at section_start_state
        @@ fun tick _ -> tick >= section_stop_at
      in
      history := PVM.remember !history section_start_at section_start_state ;
      history := PVM.remember !history section_stop_at section_stop_state ;
      let stop_state = compress section_stop_state in
      let next_dissection =
        dissection_from_section
          !history
          branching
          {section with section_stop_state = stop_state}
      in
      let conflict_search_step =
        match next_dissection with
        | None ->
            G.Conclude
              {
                start_state = verifiable_state_at !history section_start_at;
                stop_state;
              }
        | Some next_dissection ->
            let next_dissection = List.map compress_section next_dissection in
            G.Refine {stop_state; next_dissection}
      in
      RefuteByConflict conflict_search_step
    in
    let next_move dissection =
      let (move, history') = next_move !history branching dissection in
      history := history' ;
      move
    in
    ({initial; next_move} : _ G.client)

  let committer_from_strategy : strategy -> _ G.client = function
    | Random ->
        {
          initial =
            (fun ((section_start_at : Tick.t), start_state) ->
              let section_stop_at =
                random_tick ~from:(section_start_at :> int) ()
              in
              let section =
                random_section
                  section_start_at
                  (compress start_state)
                  section_stop_at
              in

              G.Commit section);
          next_move = random_decision;
        }
    | MachineDirected (parameters, checkpoint) ->
        machine_directed_committer parameters checkpoint

  let refuter_from_strategy : strategy -> _ G.client = function
    | Random ->
        {
          initial =
            (fun ((start_state : [`Verifiable | `Full] state), G.Commit section) ->
              let conflict_search_step =
                let next_dissection = random_dissection section in
                match next_dissection with
                | None ->
                    G.Conclude
                      {
                        start_state;
                        stop_state =
                          compress (random_state 1 (compress start_state));
                      }
                | Some next_dissection ->
                    let section = List.last section next_dissection in
                    G.Refine
                      {
                        stop_state = compress section.section_stop_state;
                        next_dissection;
                      }
              in
              G.RefuteByConflict conflict_search_step);
          next_move = random_decision;
        }
    | MachineDirected (parameters, _) -> machine_directed_refuter parameters

  let test_strategies committer_strategy refuter_strategy expectation =
    let start_state = PVM.initial_state in
    let committer = committer_from_strategy committer_strategy in
    let refuter = refuter_from_strategy refuter_strategy in
    let outcome =
      G.run ~start_at:(Tick.make 0) ~start_state ~committer ~refuter
    in
    expectation outcome

  let perfect_committer =
    MachineDirected
      ( {failing_level = 0; branching = 2; max_failure = None},
        fun tick -> (tick :> int) >= 20 + Random.int 100 )

  let perfect_refuter =
    MachineDirected
      ( {failing_level = 0; branching = 2; max_failure = None},
        fun _ -> assert false )

  let failing_committer max_failure =
    MachineDirected
      ( {failing_level = 1; branching = 2; max_failure},
        fun tick ->
          let s = match max_failure with None -> 20 | Some x -> x in
          (tick :> int) >= s )

  let failing_refuter max_failure =
    MachineDirected
      ({failing_level = 1; branching = 2; max_failure}, fun _ -> assert false)

  let commiter_wins = function
    | {G.winner = Some Committer; _} -> true
    | _ -> false

  let refuter_wins = function
    | {G.winner = Some Refuter; _} -> true
    | _ -> false

  let all_win (_ : outcome) = true
end

let perfect_perfect (module M : PVM) _max_failure =
  let module S = Strategies (MakeGame (M)) in
  S.test_strategies S.perfect_committer S.perfect_refuter S.commiter_wins

let random_random (module M : PVM) _max_failure =
  let module S = Strategies (MakeGame (M)) in
  S.test_strategies Random Random S.all_win

let random_perfect (module M : PVM) _max_failure =
  let module S = Strategies (MakeGame (M)) in
  S.test_strategies S.perfect_committer Random S.commiter_wins

let perfect_random (module M : PVM) _max_failure =
  let module S = Strategies (MakeGame (M)) in
  S.test_strategies Random S.perfect_refuter S.refuter_wins

let failing_perfect (module M : PVM) max_failure =
  let module S = Strategies (MakeGame (M)) in
  S.test_strategies
    (S.failing_committer max_failure)
    S.perfect_refuter
    S.refuter_wins

let perfect_failing (module M : PVM) max_failure =
  let module S = Strategies (MakeGame (M)) in
  S.test_strategies
    S.perfect_committer
    (S.failing_refuter max_failure)
    S.commiter_wins

let testing (f : (module PVM) -> int option -> bool) name =
  QCheck.Test.make
    ~name
    (QCheck.list_of_size QCheck.Gen.small_int (QCheck.int_range 0 100))
    (fun initial_prog ->
      QCheck.assume (initial_prog <> []) ;
      f
        (module RandomPVM (struct
          let initial_prog = initial_prog
        end))
        (Some (List.length initial_prog)))

let testing_count (f : (module PVM) -> int option -> bool) name =
  QCheck.Test.make ~name QCheck.small_int (fun target ->
      QCheck.assume (target > 0) ;
      f
        (module MakeCountingPVM (struct
          let target = target
        end))
        (Some target))

let testing_mich (f : (module PVM) -> int option -> bool) name =
  QCheck.Test.make ~name QCheck.small_int (fun _ -> f (module MPVM (Fact20)) (Some 20))

let () =
  Alcotest.run
    "Saturation"
    [
      ( "RandomPVM",
        qcheck_wrap
          [
            testing perfect_perfect "perfect-perfect";
            testing random_random "random-random";
            testing random_perfect "random-perfect";
            testing perfect_random "perfect-random";
            testing failing_perfect "failing_perfect";
            testing perfect_failing "perfect-failing";
          ] );
      ( "CountingPVM",
        qcheck_wrap
          [
            testing perfect_perfect "perfect-perfect";
            testing_count random_random "random-random";
            testing_count random_perfect "random-perfect";
            testing_count perfect_random "perfect-random";
            testing_count failing_perfect "failing_perfect";
            testing_count perfect_failing "perfect-failing";
          ] );
      ( "Fact20PVM",
        qcheck_wrap
          [
            testing perfect_perfect "perfect-perfect";
            testing_mich random_random "random-random";
            testing_mich random_perfect "random-perfect";
            testing_mich perfect_random "perfect-random";
            testing_mich failing_perfect "failing_perfect";
            testing_mich perfect_failing "perfect-failing";
          ] );
    ]

(*
let test_machine (module M : PVM) =
  let module PCG = MakeGame (M) in
  let module S = Strategies (PCG) in
  S.test ()


let () =
  test_machine
    (module RandomPVM (struct
      let initial_prog =
        QCheck.Gen.generate1
          (QCheck.Gen.list_size
             QCheck.Gen.small_int
             (QCheck.Gen.int_range 0 100))
    end)) *)
