open! Import

module Int    = Int0
module String = String0
module Array  = Array0

(* We maintain the property that all values of type [t] do not have the tag
   [double_array_tag].  Some functions below assume this in order to avoid testing the
   tag, and will segfault if this property doesn't hold. *)
type t = Caml.Obj.t array

let invariant t =
  assert (Caml.Obj.tag (Caml.Obj.repr t) <> Caml.Obj.double_array_tag);
;;

let length = Array.length

let sexp_of_t t =
  Sexp.Atom (String.concat ~sep:""
               [ "<Obj_array.t of length ";
                 Int.to_string (length t);
                 ">"
               ])
;;

let zero_obj = Caml.Obj.repr (0 : int)

(* We call [Array.create] with a value that is not a float so that the array doesn't get
   tagged with [Double_array_tag]. *)
let create_zero ~len = Array.create ~len zero_obj

let create ~len x =
  (* If we can, use [Array.create] directly. *)
  if (Caml.Obj.tag x) <> Caml.Obj.double_tag then begin
    Array.create ~len x
  end else begin
    (* Otherwise use [create_zero] and set the contents *)
    let t = create_zero ~len in
    let x = Sys.opaque_identity x in
    for i = 0 to (len - 1) do
      Array.unsafe_set t i x
    done;
    t
  end

let empty = [||]

type not_a_float = Not_a_float_0 | Not_a_float_1 of int
let _not_a_float_0 = Not_a_float_0
let _not_a_float_1 = Not_a_float_1 42

let get t i =
  (* Make the compiler believe [t] is an array not containing floats so it does not check
     if [t] is tagged with [Double_array_tag].  It is NOT ok to use [int array] since (if
     this function is inlined and the array contains in-heap boxed values) wrong register
     typing may result, leading to a failure to register necessary GC roots. *)
  Caml.Obj.repr (Array.get (Caml.Obj.magic (t : t) : not_a_float array) i : not_a_float)
;;

let unsafe_get t i =
  (* Make the compiler believe [t] is an array not containing floats so it does not check
     if [t] is tagged with [Double_array_tag]. *)
  Caml.Obj.repr
    (Array.unsafe_get (Caml.Obj.magic (t : t) : not_a_float array) i : not_a_float)
[@@inline always]
;;

let unsafe_set_int_assuming_currently_int t i int =
  (* This skips [caml_modify], which is OK if both the old and new values are integers. *)
  Array.unsafe_set (Caml.Obj.magic (t : t) : int array) i (Sys.opaque_identity int)
[@@inline always]
;;

(* For [set] and [unsafe_set], if a pointer is involved, we first do a physical-equality
   test to see if the pointer is changing.  If not, we don't need to do the [set], which
   saves a call to [caml_modify].  We think this physical-equality test is worth it
   because it is very cheap (both values are already available from the [is_int] test)
   and because [caml_modify] is expensive. *)

let set t i obj =
  (* We use [get] first but then we use [Array.unsafe_set] since we know that [i] is
     valid. *)
  let old_obj = get t i in
  if Caml.Obj.is_int old_obj && Caml.Obj.is_int obj
  then unsafe_set_int_assuming_currently_int t i (Caml.Obj.obj obj : int)
  else if not (phys_equal old_obj obj)
  then
    Array.unsafe_set t i (Sys.opaque_identity obj)
;;

let unsafe_set t i obj =
  let old_obj = unsafe_get t i in
  if Caml.Obj.is_int old_obj && Caml.Obj.is_int obj
  then unsafe_set_int_assuming_currently_int t i (Caml.Obj.obj obj : int)
  else if not (phys_equal old_obj obj)
  then Array.unsafe_set t i (Sys.opaque_identity obj)
[@@inline always]
;;

let unsafe_set_omit_phys_equal_check t i obj =
  let old_obj = unsafe_get t i in
  if Caml.Obj.is_int old_obj && Caml.Obj.is_int obj
  then unsafe_set_int_assuming_currently_int t i (Caml.Obj.obj obj : int)
  else Array.unsafe_set t i (Sys.opaque_identity obj)
[@@inline always]
;;

let singleton obj =
  create ~len:1 obj
;;

(* Pre-condition: t.(i) is an integer. *)
let unsafe_set_assuming_currently_int t i obj =
  if Caml.Obj.is_int obj
  then unsafe_set_int_assuming_currently_int t i (Caml.Obj.obj obj : int)
  else
    (* [t.(i)] is an integer and [obj] is not, so we do not need to check if they are
       equal. *)
    Array.unsafe_set t i (Sys.opaque_identity obj)
;;

let unsafe_set_int t i int =
  let old_obj = unsafe_get t i in
  if Caml.Obj.is_int old_obj
  then unsafe_set_int_assuming_currently_int t i int
  else Array.unsafe_set t i (Caml.Obj.repr (Sys.opaque_identity int))
;;

let unsafe_clear_if_pointer t i =
  let old_obj = unsafe_get t i in
  if not (Caml.Obj.is_int old_obj) then Array.unsafe_set t i (Caml.Obj.repr 0);
;;

(** [unsafe_blit] is like [Array.blit], except it uses our own for-loop to avoid
    caml_modify when possible.  Its performance is still not comparable to a memcpy. *)
let unsafe_blit ~src ~src_pos ~dst ~dst_pos ~len =
  (* When [phys_equal src dst], we need to check whether [dst_pos < src_pos] and have the
     for loop go in the right direction so that we don't overwrite data that we still need
     to read.  When [not (phys_equal src dst)], doing this is harmless.  From a
     memory-performance perspective, it doesn't matter whether one loops up or down.
     Constant-stride access, forward or backward, should be indistinguishable (at least on
     an intel i7).  So, we don't do a check for [phys_equal src dst] and always loop up in
     that case. *)
  if dst_pos < src_pos
  then
    for i = 0 to len - 1 do
      unsafe_set dst (dst_pos + i) (unsafe_get src (src_pos + i))
    done
  else
    for i = len - 1 downto 0 do
      unsafe_set dst (dst_pos + i) (unsafe_get src (src_pos + i))
    done;
;;

include
  Blit.Make
    (struct
      type nonrec t = t
      let create = create_zero
      let length = length
      let unsafe_blit = unsafe_blit
    end)
;;

let copy src =
  let dst = create_zero ~len:(length src) in
  blito ~src ~dst ();
  dst
;;

let truncate t ~len = Caml.Obj.truncate (Caml.Obj.repr (t : t)) len
