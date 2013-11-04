type key = string
type value = string

(** Gets browser cookies and returns a [(key, value) list].  *)
val get_cookie : unit -> (key * value) list

(** Sets browser cookies. Expiration time is one year by default. *)
val set_cookie : key -> value -> unit

(** Sets browser cookies with expiration time. *)
val set_cookie_with_timeout : key -> value -> Js.date Js.t -> unit

val initial_cookies : (key * value) list
