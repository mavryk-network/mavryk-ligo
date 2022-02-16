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

open Compare.Int
open Sc_rollup_PVM_sem

let repeat n f = List.init ~when_negative_length:[] n f

module Make (P : S) = struct
  module PVM = P

  type history = PVM.state Tick_repr.Map.t

  let remember history tick state = Tick_repr.Map.add tick state history

  let execute_until tick state pred =
    let rec loop state tick =
      if pred tick state then (tick, state)
      else
        match PVM.eval state with
        | None -> (tick, state)
        | Some state -> loop state (Tick_repr.next tick)
    in
    loop state tick

  let state_at history tick initial_state =
    let (lower, ostate, _) = Tick_repr.Map.split tick history in
    match ostate with
    | Some state -> state
    | None ->
        let (tick0, state0) =
          match Tick_repr.Map.max_binding lower with
          | Some (t, s) -> (t, s)
          | None -> (Tick_repr.initial, initial_state)
        in
        snd
          (execute_until tick0 state0 (fun tick' _ -> Tick_repr.(tick' = tick)))

  module Section_repr = struct
    type section = {
      section_start_state : PVM.state;
      section_start_at : Tick_repr.t;
      section_stop_state : PVM.state;
      section_stop_at : Tick_repr.t;
    }

    and dissection = section Tick_repr.Map.t

    let section_encoding =
      let open Data_encoding in
      conv
        (fun {
               section_start_state;
               section_start_at;
               section_stop_state;
               section_stop_at;
             } ->
          ( section_start_state,
            section_start_at,
            section_stop_state,
            section_stop_at ))
        (fun ( section_start_state,
               section_start_at,
               section_stop_state,
               section_stop_at ) ->
          {
            section_start_state;
            section_start_at;
            section_stop_state;
            section_stop_at;
          })
        (obj4
           (req "section_start_state" PVM.encoding)
           (req "section_start_at" Tick_repr.encoding)
           (req "section_stop_state" PVM.encoding)
           (req "section_stop_at" Tick_repr.encoding))

    let dissection_encoding =
      let open Data_encoding in
      let open Tick_repr in
      option
      @@ conv
           (fun map -> List.of_seq @@ Map.to_seq map)
           (fun list -> Map.of_seq @@ List.to_seq list)
           (list @@ tup2 encoding section_encoding)

    let find_section section (dissection : dissection) =
      let open Tick_repr in
      Option.bind
        (Map.find section.section_start_at dissection)
        (fun {section_start_at; section_stop_at; _} ->
          if
            section_start_at = section.section_start_at
            && section_stop_at = section.section_stop_at
          then Some section
          else None)

    let pp_of_section ppf (s : section) =
      Format.fprintf
        ppf
        "( %a ) -- [%a] \n ->\n  ( %a ) -- [%a]\n "
        PVM.pp
        s.section_start_state
        Tick_repr.pp
        s.section_start_at
        PVM.pp
        s.section_stop_state
        Tick_repr.pp
        s.section_stop_at

    let pp_of_dissection ppf d =
      let open Tick_repr in
      let list = List.of_seq @@ Map.to_seq d in
      Format.pp_print_list
        ~pp_sep:(fun ppf () -> Format.pp_print_string ppf ";\n\n")
        (fun ppf (key, b) ->
          Format.fprintf ppf "key = %a \n val = %a\n" pp key pp_of_section b)
        ppf
        list

    let pp_optional_dissection d =
      Format.pp_print_option
        ~none:(fun ppf () ->
          Format.pp_print_text ppf "no dissection at the moment")
        pp_of_dissection
        d

    let valid_section ({section_start_at; section_stop_at; _} : section) =
      Tick_repr.(section_stop_at > section_start_at)

    exception Dissection_error of string

    let valid_dissection section dissection =
      let open Tick_repr in
      let aux section dissection =
        Map.fold
          (fun _ v acc ->
            if acc = v.section_start_at && valid_section v then
              v.section_stop_at
            else raise (Dissection_error "invalid dissection"))
          dissection
          section.section_start_at
      in
      try section.section_stop_at = aux section dissection with _ -> false

    let dissection_of_section history (branching : int) (section : section) =
      if Tick_repr.(next section.section_start_at = section.section_stop_at)
      then None
      else
        let start = Tick_repr.to_int section.section_start_at in
        let stop = Tick_repr.to_int section.section_stop_at in
        let len = stop - start in
        let branching = min len branching in
        let bucket = len / branching in
        let reminder = len mod branching in
        let rec aux index branch rem map =
          if index = branch then map
          else
            let start_at = start + (bucket * index) + min rem index in
            let stop_at =
              start + (bucket * (index + 1)) + min rem (index + 1)
            in
            let section_start_at =
              Option.value
                ~default:Tick_repr.initial
                (Tick_repr.of_int start_at)
            and section_stop_at =
              Option.value ~default:Tick_repr.initial (Tick_repr.of_int stop_at)
            in
            let section =
              {
                section_start_at;
                section_start_state =
                  state_at history section_start_at section.section_start_state;
                section_stop_at;
                section_stop_state =
                  state_at history section_stop_at section.section_start_state;
              }
            in
            let new_map =
              Tick_repr.Map.add section.section_start_at section map
            in
            aux (index + 1) branching rem new_map
        in

        let (dissection : dissection) =
          aux 0 branching reminder Tick_repr.Map.empty
        in
        Some dissection
  end

  type player = Committer | Refuter

  let pp_of_player ppf player =
    Format.fprintf
      ppf
      (match player with Committer -> "committer" | Refuter -> "refuter")

  let player_encoding =
    let open Data_encoding in
    union
      ~tag_size:`Uint8
      [
        case
          ~title:"Commiter"
          (Tag 0)
          string
          (function Committer -> Some "committer" | _ -> None)
          (fun _ -> Committer);
        case
          ~title:"Refuter"
          (Tag 1)
          string
          (function Refuter -> Some "refuter" | _ -> None)
          (fun _ -> Refuter);
      ]

  let opponent = function Committer -> Refuter | Refuter -> Committer

  type t = {
    turn : player;
    start_state : PVM.state;
    start_at : Tick_repr.t;
    player_stop_state : PVM.state;
    opponent_stop_state : PVM.state;
    stop_at : Tick_repr.t;
    current_dissection : Section_repr.dissection option;
  }

  let encoding =
    let open Data_encoding in
    conv
      (fun {
             turn;
             start_state;
             start_at;
             player_stop_state;
             opponent_stop_state;
             stop_at;
             current_dissection;
           } ->
        ( turn,
          start_state,
          start_at,
          player_stop_state,
          opponent_stop_state,
          stop_at,
          current_dissection ))
      (fun ( turn,
             start_state,
             start_at,
             player_stop_state,
             opponent_stop_state,
             stop_at,
             current_dissection ) ->
        {
          turn;
          start_state;
          start_at;
          player_stop_state;
          opponent_stop_state;
          stop_at;
          current_dissection;
        })
      (obj7
         (req "turn" player_encoding)
         (req "start_state" PVM.encoding)
         (req "start_at" Tick_repr.encoding)
         (req "player_stop_state" PVM.encoding)
         (req "oponent_stop_state" PVM.encoding)
         (req "stop_at" Tick_repr.encoding)
         (req "current_dissection" Section_repr.dissection_encoding))

  type conflict_search_step =
    | Refine of {
        stop_state : PVM.state;
        next_dissection : Section_repr.dissection;
      }
    | Conclude : {
        start_state : PVM.state;
        stop_state : PVM.state;
      }
        -> conflict_search_step

  type move =
    | ConflictInside of {
        choice : Section_repr.section;
        conflict_search_step : conflict_search_step;
      }

  type commit = Commit of Section_repr.section

  type refutation = RefuteByConflict of conflict_search_step

  type reason = InvalidMove | ConflictResolved

  let pp_of_reason ppf reason =
    Format.fprintf
      ppf
      "%s"
      (match reason with
      | InvalidMove -> "invalid move"
      | ConflictResolved -> "conflict resolved")

  type outcome = {winner : player option; reason : reason}

  let pp_of_winner winner =
    Format.pp_print_option
      ~none:(fun ppf () -> Format.pp_print_text ppf "no winner")
      pp_of_player
      winner

  let pp_of_outcome ppf {winner; reason} =
    Format.fprintf
      ppf
      "%a because of %a"
      pp_of_winner
      winner
      pp_of_reason
      reason

  type state = Over of outcome | Ongoing of t

  let pp_of_game ppf (g : t) =
    Format.fprintf
      ppf
      "%a @ %a -> %a / %a @ %a [%a] %s playing"
      PVM.pp
      g.start_state
      Tick_repr.pp
      g.start_at
      PVM.pp
      g.player_stop_state
      PVM.pp
      g.opponent_stop_state
      Tick_repr.pp
      g.stop_at
      Section_repr.pp_optional_dissection
      g.current_dissection
      (match g.turn with Committer -> "committer" | Refuter -> "refuter")

  let pp_of_move ppf = function
    | ConflictInside
        {choice; conflict_search_step = Refine {next_dissection; stop_state}} ->
        Format.fprintf
          ppf
          "conflict is inside %a, should end with %a, new dissection = %a"
          Section_repr.pp_of_section
          choice
          PVM.pp
          stop_state
          Section_repr.pp_of_dissection
          next_dissection
    | ConflictInside
        {choice; conflict_search_step = Conclude {start_state; stop_state}} ->
        Format.fprintf
          ppf
          "atomic conflict found inside %a, we can verify that it starts with \
           %a and should end with %a"
          Section_repr.pp_of_section
          choice
          PVM.pp
          start_state
          PVM.pp
          stop_state

  let conflict_found (game : t) =
    Compare.Int.(
      Tick_repr.to_int game.stop_at - Tick_repr.to_int game.start_at = 1)

  let stop_state = function
    | Refine {stop_state; _} -> stop_state
    | Conclude {stop_state; _} -> stop_state

  let initial (Commit commit) (refutation : conflict_search_step) =
    let game =
      {
        start_state = commit.section_start_state;
        start_at = commit.section_start_at;
        opponent_stop_state = commit.section_stop_state;
        stop_at = commit.section_stop_at;
        player_stop_state = stop_state refutation;
        current_dissection = None;
        turn = Refuter;
      }
    in
    let choice = commit in
    let move = ConflictInside {choice; conflict_search_step = refutation} in
    (game, move)

  let resolve_conflict (game : t) verifiable_start_state =
    let open PVM in
    assert (conflict_found game) ;
    let player = game.turn in
    let over winner = {winner; reason = ConflictResolved} in
    match eval verifiable_start_state with
    | Some stop_state -> (
        let player_state_valid =
          PVM.equal_states stop_state game.player_stop_state
        in
        let opponent_state_valid =
          PVM.equal_states stop_state game.opponent_stop_state
        in
        match (player_state_valid, opponent_state_valid) with
        | (true, true) -> over @@ Some Committer
        | (true, false) -> over @@ Some player
        | (false, true) -> over @@ Some (opponent player)
        | (false, false) -> over @@ None)
    | None -> over @@ None

  let apply_choice ~(game : t) ~(choice : Section_repr.section)
      chosen_stop_state =
    Option.bind
      (match game.current_dissection with
      | Some dissection ->
          Section_repr.find_section choice dissection
          (* TODO faster binary search in the list if we have binary tree.*)
      | None ->
          if PVM.equal_states choice.section_start_state game.start_state then
            Some choice
          else None)
    @@ fun ({
              section_start_state;
              section_start_at;
              section_stop_state;
              section_stop_at;
            } :
             Section_repr.section) ->
    if PVM.equal_states chosen_stop_state section_stop_state then None
    else
      Some
        {
          game with
          start_state = section_start_state;
          start_at = section_start_at;
          opponent_stop_state = section_stop_state;
          player_stop_state = chosen_stop_state;
          stop_at = section_stop_at;
        }

  let apply_dissection ~(game : t) (next_dissection : Section_repr.dissection) =
    let current_section : Section_repr.section =
      {
        section_start_state = game.start_state;
        section_start_at = game.start_at;
        section_stop_state = game.opponent_stop_state;
        section_stop_at = game.stop_at;
      }
    in
    if Section_repr.valid_dissection current_section next_dissection then
      Some {game with current_dissection = Some next_dissection}
    else None

  let verifiable_representation vstate state =
    if PVM.equal_states vstate state then Some () else None

  let play game (ConflictInside {choice; conflict_search_step}) =
    let player = game.turn in

    let apply_move () =
      match conflict_search_step with
      | Refine {next_dissection; stop_state} ->
          Option.bind (apply_choice ~game ~choice stop_state) @@ fun game ->
          Option.bind (apply_dissection ~game next_dissection) @@ fun game ->
          Some (Ongoing game)
      | Conclude {start_state; stop_state} ->
          Option.bind (apply_choice ~game ~choice stop_state) @@ fun game ->
          Option.bind (verifiable_representation start_state game.start_state)
          @@ fun () ->
          if conflict_found game then
            Some (Over (resolve_conflict game start_state))
          else None
    in
    match apply_move () with
    | None -> Over {winner = Some (opponent player); reason = InvalidMove}
    | Some state -> state

  type ('from, 'initial) client = {
    initial : 'from -> 'initial;
    next_move : Section_repr.dissection -> move;
  }

  let run ~start_at ~(start_state : PVM.state) ~committer ~refuter =
    let (Commit commit) = committer.initial (start_at, start_state) in
    let (RefuteByConflict refutation) =
      refuter.initial (start_state, Commit commit)
    in
    let outcome =
      let rec loop game move =
        match play game move with
        | Over outcome -> outcome
        | Ongoing game ->
            let game = {game with turn = opponent game.turn} in
            let move =
              match game.turn with
              | Committer ->
                  let nm =
                    committer.next_move
                      (Option.value
                         ~default:Tick_repr.Map.empty
                         game.current_dissection)
                  in
                  nm
              | Refuter ->
                  refuter.next_move
                    (Option.value
                       ~default:Tick_repr.Map.empty
                       game.current_dissection)
            in
            loop game move
      in
      let (game, move) = initial (Commit commit) refutation in
      loop game move
    in
    outcome
end
