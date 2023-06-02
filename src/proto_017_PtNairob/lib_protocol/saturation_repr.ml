(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

(* let () = assert (Sys.int_size = 63) *)

type _ t = Z.t

type mul_safe

type may_saturate

let may_saturate : _ t -> may_saturate t = fun x -> x

let to_int = Z.to_int
let wont_saturate : may_saturate t -> int = fun x -> Z.to_int x

let ( < ) : _ t -> _ t -> bool = Z.lt

let ( <= ) : _ t -> _ t -> bool = Z.leq

let ( > ) : _ t -> _ t -> bool = Z.gt

let ( >= ) : _ t -> _ t -> bool = Z.geq

let ( = ) : _ t -> _ t -> bool = Z.equal

let equal = ( = )

let ( <> ) (x: _ t) (y: _ t) : bool = not @@ equal x y

let max : _ t -> _ t -> _ t = fun x y -> if x >= y then x else y

let min : _ t -> _ t -> _ t = fun x y -> if x >= y then y else x

let compare (x: _ t) (y:_ t) : int = Z.compare x y

let saturated = Z.of_int max_int
let saturated_int = max_int


let ( >! ) (x : _ t) (y : int) : bool = Z.(x > (Z.of_int y))

let of_int_opt (t: int): _ t option =
  let open Z in
  let z = Z.of_int t in
  if z >= zero && Z.(z < saturated) then Some z else None

let of_z_opt z = Some z

let to_z x = x

let saturate_if_undef = function None -> saturated | Some x -> x

let safe_z z = saturate_if_undef @@ of_z_opt z

let safe_int x = of_int_opt x |> saturate_if_undef

let numbits = Z.numbits

let zero = Z.zero

let one = Z.one

let small_enough z =
  let open Z in
  equal (logand z (of_int 0x7fffffff80000000)) zero

let mul_safe x = if small_enough x then Some x else None

let mul_safe_exn x =
  if small_enough x then x
  else failwith (Format.sprintf "mul_safe_exn: %d must be below 2147483648" (Z.to_int x))


let mul_safe_of_int_exn x =
  Option.bind (of_int_opt x) mul_safe |> function
  | None ->
      failwith
        (Format.sprintf "mul_safe_of_int_exn: %d must be below 2147483648" x)
  | Some x -> x

(* If [x] is positive, shifting to the right will produce a number
   which is positive and is less than [x]. *)
let shift_right = Z.shift_right

let shift_left x y =
  let open Z in
  if shift_right saturated y < x then saturated else Z.shift_left x y

let mul x y =
  let open Z in
  (* assert (x >= 0 && y >= 0); *)
  match x with
  | x when x = Z.zero -> zero
  | x ->
     if small_enough x && small_enough y then Z.mul x y
      else if Z.gt y (Z.div saturated x) then saturated
      else Z.mul x y

let mul_fast x y = Z.mul x y

let scale_fast x y =
  let open Z in
  if x = zero then zero
  else if small_enough y then Z.mul x y
  else if Z.gt y (Z.div saturated  x) then saturated
  else Z.mul x y

let add x y =
  let z = Z.add x y in
  if (Z.geq z zero) then z else saturated

let succ x = add one x

let sub x y = Z.max (Z.sub x y) zero

let sub_opt x y =
  let s = Z.sub x y in
  if (Z.geq s zero) then Some s else None

(* Notice that Z.erem does not behave as mod on negative numbers.
   Fortunately, the inhabitant of [t] are non-negative. *)
let erem x y = Z.rem x y

let ediv x y = Z.ediv x  y

let sqrt = Z.sqrt

let t_to_z_exn z =
  match of_z_opt z with
  | None ->
      (* since the encoding is applied to values of type [t]. *) assert false
  | Some x -> x

let z_encoding = Data_encoding.(check_size 9 (conv to_z t_to_z_exn z))

let n_encoding = Data_encoding.(check_size 9 (conv to_z t_to_z_exn n))

let pp = Z.pp_print 

module Syntax = struct
  (* This is a good enough approximation. S.log2 0 = 1 *)
  let log2 x = safe_int (1 + numbits x)

  let sqrt = sqrt

  let ( + ) = add

  let ( - ) = sub

  let ( * ) = mul

  let ( < ) = ( < )

  let ( = ) = ( = )

  let ( lsr ) = shift_right

  let ( lsl ) = shift_left
end
