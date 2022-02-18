(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2022 Trili Tech, <contact@trili.tech>                       *)
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
open Protocol

exception TickNotFound of Tick_repr.t

open Lib_test.Qcheck_helpers

(**
Helpers
*)
let option_get = function
  | Some a -> a
  | None -> raise (Invalid_argument "option is None")

module type TestPVM = sig
  include Sc_rollup_PVM_sem.S

  module Internal_for_tests : sig
    val initial_state : state

    val random_state : int -> state -> state
  end
end

(** this is avery simple PVM whose state is an integer and who can count up to a certin target.*)
module MakeCountingPVM (P : sig
  val target : int
end) : TestPVM with type state = int = struct
  type state = int

  let compress x = x

  module Internal_for_tests = struct
    let initial_state = 0

    let random_state _ _ = Random.bits ()
  end

  let equal_states = ( = )

  let pp ppf = Format.fprintf ppf "%d"

  let encoding = Data_encoding.int16

  let eval state = if state >= P.target then Some state else Some (state + 1)
end

let operation state number =
  Digest.bytes @@ Bytes.of_string @@ state ^ string_of_int number

(** this is a simple  "random PVM" its state is a pair (state, prog) whee state is a string and 
prog is a list of integers representing the a program. evaluation just consumes the head of program 
and modifies state by concatenating the head of the program to the existing state and hashing the result.
It is initiated with an initialprgram that creates the initial state. *)
module MakeRandomPVM (P : sig
  val initial_prog : int list
end) : TestPVM with type state = string * int list = struct
  type state = string * int list

  let compress x = x

  module Internal_for_tests = struct
    let initial_state = ("hello", P.initial_prog)

    let random_state length ((_, program) : state) =
      let remaining_program = TzList.drop_n length program in
      let (stop_state : state) =
        (operation "" (Random.bits ()), remaining_program)
      in
      stop_state
  end

  let equal_states = ( = )

  let pp ppf (st, li) =
    Format.fprintf ppf "%s@ %a" st (Format.pp_print_list Format.pp_print_int) li

  let encoding =
    let open Data_encoding in
    conv
      (fun (value, list) -> (value, list))
      (fun (value, list) -> (value, list))
      (tup2 string (list int16))

  let eval (hash, continuation) =
    match continuation with [] -> None | h :: tl -> Some (operation hash h, tl)
end

(** This module introduces some testing strategies for a game created from a PVM
*)
module Strategies (P : TestPVM) = struct
  module Game = Game_repr.Make (P)
  open Game
  open PVM

  (** this eception is needed for a small that alows a "fold-with break"
*)
  exception Section of Game.Section_repr.section

  let random_tick ?(from = 0) () =
    Option.value
      ~default:Tick_repr.initial
      (Tick_repr.of_int (from + Random.int 31))

  (** this picks a random section between start_at and stop_at. The states 
  are determined by the random_state function.*)
  let random_section (start_at : Tick_repr.t) start_state
      (stop_at : Tick_repr.t) =
    let x = min 10000 @@ Z.to_int (Tick_repr.distance start_at stop_at) in
    let length = 1 + try Random.int x with _ -> 0 in
    let stop_at = Tick_repr.(of_int (to_int start_at + length)) in

    ({
       section_start_at = start_at;
       section_start_state = start_state;
       section_stop_at = Option.value ~default:start_at stop_at;
       section_stop_state = P.Internal_for_tests.random_state length start_state;
     }
      : Section_repr.section)

  (** this picks a random dissection of a given section. 
  The sections involved are random and their states have no connection with the initial section.*)
  let random_dissection (gsection : Section_repr.section) =
    let open Tick_repr in
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
            (Map.add section.section_start_at section dissection)
            section.section_stop_at
            section.section_stop_state
    in
    if
      Compare.Int.(
        Z.to_int (distance gsection.section_stop_at gsection.section_start_at)
        > 1)
    then
      Some
        (aux Map.empty gsection.section_start_at gsection.section_start_state)
    else None

  (** this function assmbles a random decision from a given dissection.
    It first picks a random section from the dissection and modifies randomly its states.
    If the length of this section is one tick the returns a conclusion with the given modified states.
    If the length is longer it creates a random decision and outputs a Refine decisoon with this dissection.*)
  let random_decision d =
    let open Section_repr in
    let open Tick_repr.Map in
    let cardinal = cardinal d in
    let x = Random.int cardinal in
    (* Section_repr.pp_of_dissection Format.std_formatter d; *)
    let (_, section) =
      try
        fold
          (fun _ s (n, _) -> if n = x then raise (Section s) else (n + 1, None))
          d
          (0, None)
      with Section sec -> (0, Some sec)
    in
    let section =
      match section with None -> raise Not_found | Some section -> section
    in
    let section_start_at = section.section_start_at in
    let section_stop_at = section.section_stop_at in
    let section_start_state =
      P.Internal_for_tests.random_state 0 section.section_start_state
    in
    let section_stop_state =
      P.Internal_for_tests.random_state
        Tick_repr.(to_int section_stop_at - to_int section_start_at)
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
          Conclude
            {
              start_state = section.section_start_state;
              stop_state = section.section_stop_state;
            }
      | Some next_dissection ->
          Refine {stop_state = section.section_stop_state; next_dissection}
    in
    ConflictInside {choice = section; conflict_search_step}

  (** technical params for machine directed strategies, branching is the number of pieces for a dissection
failing level*)
  type parameters = {
    branching : int;
    failing_level : int;
    max_failure : int option;
  }

  type checkpoint = Tick_repr.t -> bool

  (** there are two kinds of strategies, random and machine dirrected by a params and a checkpoint*)
  type strategy = Random | MachineDirected of parameters * checkpoint

  (**checks that the stop state of a section conflicts with the one in the history.*)
  let conflicting_section (history : history) (section : Section_repr.section) =
    not
      (equal_states
         section.section_stop_state
         (state_at history section.section_stop_at section.section_start_state))

  (** updates the history*)
  let remember_section history (section : Section_repr.section) =
    let history =
      remember history section.section_start_at section.section_start_state
    in
    remember history section.section_stop_at section.section_stop_state

  (** Finds the section (if it exists) is a dissection that conflicts with the history. 
    This is where the trick with the exception appears*)
  let find_conflict history dissection =
    try
      Tick_repr.Map.fold
        (fun _ v _ ->
          if conflicting_section history v then raise (Section v) else None)
        dissection
        None
    with Section section -> Some section

  (** [next_move history branching dissection] 
  If finds the next move based on a dissection and history.
  It finds the first section of dissection that conflicts with the history. 
  If the section has length one tick it returns a move with a Conclude conflict_search_step.
  If the section is longer it creates a new dissection with branching many pieces, updates the history based on 
  this dissection and returns a move with a Refine type conflict_search_step.
   *)
  let next_move history branching dissection =
    let section =
      find_conflict history dissection |> function
      | None -> raise (TickNotFound Tick_repr.initial)
      | Some s -> s
    in
    let next_dissection =
      Section_repr.dissection_of_section history branching section
    in
    let (empty_history : history) = Tick_repr.Map.empty in
    let (conflict_search_step, history) =
      match next_dissection with
      | None ->
          let stop_state =
            state_at
              history
              (Tick_repr.next section.section_start_at)
              P.Internal_for_tests.initial_state
          in
          ( Conclude
              {
                start_state =
                  Game.(
                    state_at
                      history
                      section.section_start_at
                      P.Internal_for_tests.initial_state);
                stop_state;
              },
            empty_history )
      | Some next_dissection ->
          let stop_state =
            state_at
              history
              section.section_stop_at
              P.Internal_for_tests.initial_state
          in
          let history =
            Tick_repr.Map.fold
              (fun _ v acc -> remember_section acc v)
              next_dissection
              empty_history
          in
          (Refine {stop_state; next_dissection}, history)
    in
    (ConflictInside {choice = section; conflict_search_step}, history)

  (** this is an outomatic commuter client. It generates a "perfect" client for the commuter unless 
given some positive failing level when it "forgets" to evaluate the ticks on the autmatically generated failure list.*)
  let machine_directed_committer {branching; _} pred =
    let (history : history ref) = ref Tick_repr.Map.empty in
    let initial ((section_start_at : Tick_repr.t), section_start_state) : commit
        =
      let (section_stop_at, section_stop_state) =
        Game.execute_until section_start_at section_start_state @@ fun tick _ ->
        pred tick
      in
      history := Game.remember !history section_start_at section_start_state ;
      history := Game.remember !history section_stop_at section_stop_state ;
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

    ({initial; next_move} : _ client)

  (** this is an outomatic refuter client. It generates a "perfect" client for the commuter unless 
given some positive failing level when it "forgets" to evaluate the ticks on the autmatically generated failure list.*)
  let machine_directed_refuter {branching; _} =
    let (history : history ref) = ref Tick_repr.Map.empty in
    let initial (section_start_state, Commit section) : refutation =
      let ({section_start_at; section_stop_at; _} : Section_repr.section) =
        section
      in
      let (_stop_at, section_stop_state) =
        Game.execute_until section_start_at section_start_state @@ fun tick _ ->
        tick >= section_stop_at
      in
      history := Game.remember !history section_start_at section_start_state ;
      history := Game.remember !history section_stop_at section_stop_state ;
      let next_dissection =
        Section_repr.dissection_of_section
          !history
          branching
          {section with section_stop_state}
      in
      let conflict_search_step =
        match next_dissection with
        | None ->
            Conclude
              {
                start_state =
                  Game.state_at
                    !history
                    section_start_at
                    P.Internal_for_tests.initial_state;
                stop_state = section_stop_state;
              }
        | Some next_dissection ->
            Refine {stop_state = section_stop_state; next_dissection}
      in
      RefuteByConflict conflict_search_step
    in
    let next_move dissection =
      let (move, history') = next_move !history branching dissection in
      history := history' ;
      move
    in
    ({initial; next_move} : _ client)

  (** This builds a commiter client from a strategy. 
    If the strategy is MachineDirected it uses the above constructions. 
    if the strategy is random then it usesa random section for the initial commitments 
     and  the random decision for the next move.*)
  let committer_from_strategy : strategy -> _ client = function
    | Random ->
        {
          initial =
            (fun ((section_start_at : Tick_repr.t), start_state) ->
              let section_stop_at =
                random_tick ~from:(Tick_repr.to_int section_start_at) ()
              in
              let section =
                random_section section_start_at start_state section_stop_at
              in
              Commit section);
          next_move = random_decision;
        }
    | MachineDirected (parameters, checkpoint) ->
        machine_directed_committer parameters checkpoint

  (** This builds a refuter client from a strategy. 
    If the strategy is MachineDirected it uses the above constructions. 
    If the strategy is random then it uses a randomdissection 
    of the commited section for the initial refutation 
     and  the random decision for the next move.*)
  let refuter_from_strategy : strategy -> _ client = function
    | Random ->
        {
          initial =
            (fun ((start_state : state), Commit section) ->
              let conflict_search_step =
                let next_dissection = random_dissection section in
                match next_dissection with
                | None ->
                    Conclude
                      {
                        start_state;
                        stop_state =
                          P.Internal_for_tests.random_state 1 start_state;
                      }
                | Some next_dissection ->
                    let (_, section) =
                      Option.value
                        ~default:(Tick_repr.initial, section)
                        (Tick_repr.Map.max_binding next_dissection)
                    in
                    Refine
                      {stop_state = section.section_stop_state; next_dissection}
              in
              RefuteByConflict conflict_search_step);
          next_move = random_decision;
        }
    | MachineDirected (parameters, _) -> machine_directed_refuter parameters

  (** [test_strategies committer_strategy refuter_strategy expectation] 
    runs a game based oin the two given strategies and checks that the resulting 
    outcome fits the expectations. *)
  let test_strategies committer_strategy refuter_strategy expectation =
    let start_state = P.Internal_for_tests.initial_state in
    let committer = committer_from_strategy committer_strategy in
    let refuter = refuter_from_strategy refuter_strategy in
    let outcome =
      run ~start_at:Tick_repr.initial ~start_state ~committer ~refuter
    in
    expectation outcome

  (** This is a commuter client having a perfect strategy*)
  let perfect_committer =
    MachineDirected
      ( {failing_level = 0; branching = 2; max_failure = None},
        fun tick -> Tick_repr.to_int tick >= 20 + Random.int 100 )
  (** This is a refuter client having a perfect strategy*)

  let perfect_refuter =
    MachineDirected
      ( {failing_level = 0; branching = 2; max_failure = None},
        fun _ -> assert false )

  (** This is a commuter client having a strategy that forgets a tick*)
  let failing_committer max_failure =
    MachineDirected
      ( {failing_level = 1; branching = 2; max_failure},
        fun tick ->
          let s = match max_failure with None -> 20 | Some x -> x in
          Tick_repr.to_int tick >= s )

  (** This is a commuter client having a strategy that forgets a tick*)
  let failing_refuter max_failure =
    MachineDirected
      ({failing_level = 1; branching = 2; max_failure}, fun _ -> assert false)

  (** the possible expectation functions *)
  let commiter_wins = function
    | {winner = Some Committer; _} -> true
    | _ -> false

  let refuter_wins = function {winner = Some Refuter; _} -> true | _ -> false

  let all_win (_ : outcome) = true
end

(** the following are the possible combinations of strategies*)
let perfect_perfect (module P : TestPVM) _max_failure =
  let module R = Strategies (P) in
  R.test_strategies R.perfect_committer R.perfect_refuter R.commiter_wins

let random_random (module P : TestPVM) _max_failure =
  let module S = Strategies (P) in
  S.test_strategies Random Random S.all_win

let random_perfect (module P : TestPVM) _max_failure =
  let module S = Strategies (P) in
  S.test_strategies Random S.perfect_refuter S.refuter_wins

let perfect_random (module P : TestPVM) _max_failure =
  let module S = Strategies (P) in
  S.test_strategies S.perfect_committer Random S.commiter_wins

let failing_perfect (module P : TestPVM) max_failure =
  let module S = Strategies (P) in
  S.test_strategies
    (S.failing_committer max_failure)
    S.perfect_refuter
    S.refuter_wins

let perfect_failing (module P : TestPVM) max_failure =
  let module S = Strategies (P) in
  S.test_strategies
    S.perfect_committer
    (S.failing_refuter max_failure)
    S.commiter_wins

(** this assembles a test from a RandomPVM and a function that choses the type of strategies *)
let testing_randomPVM (f : (module TestPVM) -> int option -> bool) name =
  QCheck.Test.make
    ~name
    (QCheck.list_of_size QCheck.Gen.small_int (QCheck.int_range 0 100))
    (fun initial_prog ->
      QCheck.assume (initial_prog <> []) ;
      f
        (module MakeRandomPVM (struct
          let initial_prog = initial_prog
        end))
        (Some (List.length initial_prog)))

(** this assembles a test from a CountingPVM and a function that choses the type of strategies *)
let testing_countPVM (f : (module TestPVM) -> int option -> bool) name =
  QCheck.Test.make ~name QCheck.small_int (fun target ->
      QCheck.assume (target > 0) ;
      f
        (module MakeCountingPVM (struct
          let target = target
        end))
        (Some target))

(** this assembles a test from a MichelsonPVM and a function that choses the type of strategies *)

(* let testing_mich (f : (module TestPVM) -> int option -> bool) name =
  QCheck.Test.make ~name QCheck.small_int (fun _ ->
      f (module MakeMPVM (Fact20)) (Some 20)) *)

let test_random_dissection (module P : TestPVM) start_at length branching =
  let open P in
  let module S = Strategies (P) in
  let section_start_state = Internal_for_tests.initial_state in
  let section_stop_at =
    match Tick_repr.of_int (start_at + length) with
    | None -> assert false
    | Some x -> x
  in
  let section_start_at =
    match Tick_repr.of_int start_at with None -> assert false | Some x -> x
  in

  let section =
    S.Game.Section_repr.
      {
        section_start_at;
        section_start_state;
        section_stop_at;
        section_stop_state =
          Internal_for_tests.random_state length section_start_state;
      }
  in
  let option_dissection =
    let empty_history = Tick_repr.Map.empty in
    S.Game.Section_repr.dissection_of_section empty_history branching section
  in
  let dissection =
    match option_dissection with
    | None -> raise (Invalid_argument "no dissection")
    | Some x -> x
  in
  S.Game.Section_repr.valid_dissection section dissection

let testDissection =
  [
    QCheck.Test.make
      ~name:"randomVPN"
      (QCheck.quad
         (QCheck.list_of_size QCheck.Gen.small_int (QCheck.int_range 0 100))
         QCheck.small_int
         QCheck.small_int
         QCheck.small_int)
      (fun (initial_prog, start_at, length, branching) ->
        QCheck.assume
          (start_at >= 0 && length > 1
          && List.length initial_prog > start_at + length
          && 1 < branching) ;
        let module P = MakeRandomPVM (struct
          let initial_prog = initial_prog
        end) in
        test_random_dissection (module P) start_at length branching);
    QCheck.Test.make
      ~name:"count"
      (QCheck.quad
         QCheck.small_int
         QCheck.small_int
         QCheck.small_int
         QCheck.small_int)
      (fun (target, start_at, length, branching) ->
        QCheck.assume (start_at >= 0 && length > 1 && 1 < branching) ;
        let module P = MakeCountingPVM (struct
          let target = target
        end) in
        test_random_dissection (module P) start_at length branching);
  ]

let () =
  Alcotest.run
    "Refutation Game"
    [
      ("Dissection tests", qcheck_wrap testDissection);
      ( "RandomPVM",
        qcheck_wrap
          [
            testing_randomPVM perfect_perfect "perfect-perfect";
            testing_randomPVM random_random "random-random";
            testing_randomPVM random_perfect "random-perfect";
            testing_randomPVM perfect_random "perfect-random";
          ] );
      ( "CountingPVM",
        qcheck_wrap
          [
            testing_countPVM perfect_perfect "perfect-perfect";
            testing_countPVM random_random "random-random";
            testing_countPVM random_perfect "random-perfect";
            testing_countPVM perfect_random "perfect-random";
          ] );
    ]
