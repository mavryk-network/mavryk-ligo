(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 TriliTech <contact@trili.tech>                         *)
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

open Tezos_scoru_wasm
module Wasmer = Tezos_wasmer
module Lazy_containers = Tezos_lazy_containers

let to_ref_memory mem =
  let open Wasmer.Memory in
  let get_chunk page_id =
    let start_address =
      Int64.mul page_id Lazy_containers.Chunked_byte_vector.Chunk.size
    in
    let body =
      Bytes.init
        (Int64.to_int Lazy_containers.Chunked_byte_vector.Chunk.size)
        (fun i ->
          Wasmer.Memory.get mem Int64.(add start_address (of_int i) |> to_int)
          |> Unsigned.UInt8.to_int |> Char.chr)
    in
    Lwt.return (Lazy_containers.Chunked_byte_vector.Chunk.of_bytes body)
  in
  let length = Wasmer.Memory.length mem |> Int64.of_int in
  let chunked = Lazy_containers.Chunked_byte_vector.create ~get_chunk length in
  let min = Unsigned.UInt32.to_int32 mem.min in
  let max = Option.map Unsigned.UInt32.to_int32 mem.max in
  Tezos_webassembly_interpreter.(
    Partial_memory.of_chunks (MemoryType {min; max}) chunked)

let commit_memory mem partial_memory =
  let chunks =
    Lazy_containers.Chunked_byte_vector.loaded_chunks
      (Tezos_webassembly_interpreter.Partial_memory.content partial_memory)
  in
  List.iter
    (function
      | chunk_id, Some chunk ->
          let start =
            Int64.mul chunk_id Lazy_containers.Chunked_byte_vector.Chunk.size
          in
          let body = Lazy_containers.Chunked_byte_vector.Chunk.to_bytes chunk in
          Bytes.iteri
            (fun i char ->
              let addr = Int64.(add start (of_int i) |> to_int) in
              Wasmer.Memory.set
                mem
                addr
                (Char.code char |> Unsigned.UInt8.of_int))
            body
      | _ -> ())
    chunks

type host_state = {
  retrieve_mem : unit -> Wasmer.Memory.t;
  buffers : Tezos_webassembly_interpreter.Eval.buffers;
  mutable durable : Durable.t;
}

let make (module Builtins : Builtins.S) state =
  let open Wasmer in
  let open Lwt.Syntax in
  let with_mem f =
    let mem = state.retrieve_mem () in
    let ref_mem = to_ref_memory mem in
    let+ value = f ref_mem in
    commit_memory mem ref_mem ;
    value
  in

  let read_input =
    fn
      (i32 @-> i32 @-> i32 @-> i32 @-> returning1 i32)
      (fun level_offset id_offset dst max_bytes ->
        with_mem @@ fun memory ->
        Host_funcs.Aux.read_input
          ~input_buffer:state.buffers.input
          ~output_buffer:state.buffers.output
          ~memory
          ~level_offset
          ~id_offset
          ~dst
          ~max_bytes)
  in
  let write_output =
    fn
      (i32 @-> i32 @-> returning1 i32)
      (fun src num_bytes ->
        with_mem @@ fun memory ->
        Host_funcs.Aux.write_output
          ~output_buffer:state.buffers.output
          ~memory
          ~src
          ~num_bytes)
  in
  let store_has =
    fn
      (i32 @-> i32 @-> returning1 i32)
      (fun key_offset key_length ->
        with_mem @@ fun memory ->
        Host_funcs.Aux.store_has
          ~durable:state.durable
          ~memory
          ~key_offset
          ~key_length)
  in
  let store_list_size =
    fn
      (i32 @-> i32 @-> returning1 i64)
      (fun key_offset key_length ->
        with_mem @@ fun memory ->
        let+ durable, result =
          Host_funcs.Aux.store_list_size
            ~durable:state.durable
            ~memory
            ~key_offset
            ~key_length
        in
        state.durable <- durable ;
        result)
  in
  let store_delete =
    fn
      (i32 @-> i32 @-> returning1 i32)
      (fun key_offset key_length ->
        with_mem @@ fun memory ->
        let+ durable, result =
          Host_funcs.Aux.store_delete
            ~durable:state.durable
            ~memory
            ~key_offset
            ~key_length
        in
        state.durable <- durable ;
        result)
  in
  let write_debug =
    fn
      (i32 @-> i32 @-> returning nothing)
      (fun key_offset key_length ->
        let mem = state.retrieve_mem () in
        let len = Wasmer.Memory.length mem |> Int32.of_int in
        let key_offset, key_length =
          match () with
          | () when key_offset >= len ->
              (* Start of key is out of bounds *)
              (0l, 0l)
          | () when key_length > Int32.sub len key_offset ->
              (* End of key would exceeds bounds *)
              (key_offset, Int32.sub len key_offset)
          | () ->
              (* Everything is ok *)
              (key_offset, key_length)
        in
        let key_offset = Int32.to_int key_offset in
        let str =
          String.init (Int32.to_int key_length) (fun i ->
              Wasmer.Memory.get mem (key_offset + i)
              |> Unsigned.UInt8.to_int |> Char.chr)
        in
        Printf.printf "DEBUG: %s\n" str ;
        Lwt.return ())
  in
  let store_copy =
    fn
      (i32 @-> i32 @-> i32 @-> i32 @-> returning1 i32)
      (fun from_key_offset from_key_length to_key_offset to_key_length ->
        with_mem @@ fun memory ->
        let+ durable, result =
          Host_funcs.Aux.store_copy
            ~durable:state.durable
            ~memory
            ~from_key_offset
            ~from_key_length
            ~to_key_offset
            ~to_key_length
        in
        state.durable <- durable ;
        result)
  in
  let store_move =
    fn
      (i32 @-> i32 @-> i32 @-> i32 @-> returning1 i32)
      (fun from_key_offset from_key_length to_key_offset to_key_length ->
        with_mem @@ fun memory ->
        let+ durable, result =
          Host_funcs.Aux.store_move
            ~durable:state.durable
            ~memory
            ~from_key_offset
            ~from_key_length
            ~to_key_offset
            ~to_key_length
        in
        state.durable <- durable ;
        result)
  in
  let store_read =
    fn
      (i32 @-> i32 @-> i32 @-> i32 @-> i32 @-> returning1 i32)
      (fun key_offset key_length value_offset dest max_bytes ->
        with_mem @@ fun memory ->
        Host_funcs.Aux.store_read
          ~durable:state.durable
          ~memory
          ~key_offset
          ~key_length
          ~value_offset
          ~dest
          ~max_bytes)
  in
  let store_write =
    fn
      (i32 @-> i32 @-> i32 @-> i32 @-> i32 @-> returning1 i32)
      (fun key_offset key_length value_offset src num_bytes ->
        with_mem @@ fun memory ->
        let+ durable, ret =
          Host_funcs.Aux.store_write
            ~durable:state.durable
            ~memory
            ~key_offset
            ~key_length
            ~value_offset
            ~src
            ~num_bytes
        in
        state.durable <- durable ;
        ret)
  in
  let store_get_nth_key =
    fn
      (i32 @-> i32 @-> i64 @-> i32 @-> i32 @-> returning1 i32)
      (fun key_offset key_length index dst max_size ->
        with_mem @@ fun memory ->
        Host_funcs.Aux.store_get_nth_key
          ~durable:state.durable
          ~memory
          ~key_offset
          ~key_length
          ~index
          ~dst
          ~max_size)
  in
  let store_value_size =
    fn
      (i32 @-> i32 @-> returning1 i32)
      (fun key_offset key_length ->
        with_mem @@ fun memory ->
        Host_funcs.Aux.store_value_size
          ~durable:state.durable
          ~memory
          ~key_offset
          ~key_length)
  in
  let reveal_preimage =
    fn
      (i32 @-> i32 @-> i32 @-> returning1 i32)
      (fun hash_addr dest max_bytes ->
        let mem = state.retrieve_mem () in
        let hash_addr = Int32.to_int hash_addr in
        let dest = Int32.to_int dest in
        let max_bytes = Int32.to_int max_bytes in
        let hash =
          String.init 32 (fun i ->
              (* XXX: No bounds checks
                 The "main" reveal function does not deal with out-of-bounds
                 scenarios. That ultimate means we don't return error codes.
                 Instead, we just fail with the exception being thrown.
                 In most cases the Fast Exec mechanism will fall back to another
                 execution mode to deal with this. *)
              Memory.get mem (hash_addr + i)
              |> Unsigned.UInt8.to_int |> Char.chr)
          |> Tezos_webassembly_interpreter.Reveal.reveal_hash_from_string_exn
        in
        let+ payload = Builtins.reveal_preimage hash in
        let revealed_bytes = min (String.length payload) max_bytes in
        let payload = String.sub payload 0 revealed_bytes in
        String.iteri
          (fun i c ->
            Char.code c |> Unsigned.UInt8.of_int |> Memory.set mem (dest + i))
          payload ;
        Int32.of_int revealed_bytes)
  in
  let reveal_metadata =
    fn
      (i32 @-> returning1 i32)
      (fun dest ->
        let mem = state.retrieve_mem () in
        let dest = Int32.to_int dest in
        let+ payload = Builtins.reveal_metadata () in
        String.iteri
          (fun i c ->
            (* XXX: Check note about bounds above please! *)
            Char.code c |> Unsigned.UInt8.of_int |> Memory.set mem (dest + i))
          payload ;
        Int32.of_int (String.length payload))
  in

  List.map
    (fun (name, impl) -> (Constants.wasm_host_funcs_virual_module, name, impl))
    [
      ("read_input", read_input);
      ("write_output", write_output);
      ("write_debug", write_debug);
      ("store_has", store_has);
      ("store_list_size", store_list_size);
      ("store_value_size", store_value_size);
      ("store_delete", store_delete);
      ("store_copy", store_copy);
      ("store_move", store_move);
      ("store_read", store_read);
      ("store_write", store_write);
      ("store_get_nth_key", store_get_nth_key);
      ("reveal_preimage", reveal_preimage);
      ("reveal_metadata", reveal_metadata);
    ]