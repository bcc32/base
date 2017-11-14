open! Import

include Array0

let raise_s = Error.raise_s

type 'a t = 'a array [@@deriving_inline compare, sexp]
let compare : 'a . ('a -> 'a -> int) -> 'a t -> 'a t -> int = compare_array
let t_of_sexp : 'a . (Sexplib.Sexp.t -> 'a) -> Sexplib.Sexp.t -> 'a t =
  array_of_sexp
let sexp_of_t : 'a . ('a -> Sexplib.Sexp.t) -> 'a t -> Sexplib.Sexp.t =
  sexp_of_array
[@@@end]

let is_sorted_strictly t ~cmp =
  let rec loop i =
    if i < 1 then
      true
    else
      cmp (get t (i - 1)) (get t i) && loop (i - 1)
      (* TODO report upstream, the below breaks *)
      (* cmp t.(i - 1) t.(i) < 0 && loop (i - 1) *)
  in
  loop (length t - 1)
;;
