(* Aliases *)
let doc = Dom_html.document
let win = Dom_html.window
let window = Dom_html.window
let loc = Js.Unsafe.variable "location"

let _s s = Js.string s
let _f f = Js.wrap_callback f

let get_element_by_id id =
  Js.Opt.get (doc##getElementById (Js.string id)) (fun () -> assert false)

let set_by_id id s =
  let div = get_element_by_id id in
  div##innerHTML <- Js.string s

let set_div_by_id id s =
  try
    set_by_id id s
  with _ -> ()

let get_by_id id =
  let div = get_element_by_id id in
  Js.to_string div##innerHTML

let get_element_by_tag_name tag =
  match (Dom.list_of_nodeList (doc##getElementsByTagName (Js.string tag ))) with
  | [] -> None
  | s :: _ -> Some s

let get_by_name tag = match get_element_by_tag_name tag with
  | None -> None
  | Some element -> begin match Js.Opt.to_option element##textContent with
      | None -> None
      | Some s -> Some (Js.to_string s) end

let read_from_input ?(msg="") ?(default="") () =
  match Js.Opt.to_option (window##prompt (_s msg, _s default)) with
    None -> assert false
  | Some s -> Js.to_string s

let jsnew0 (constr : 'a Js.t Js.constr) () =
  (Js.Unsafe.new_obj constr [| |] : 'a Js.t)

let jsnew1 (constr : ('a -> 'z Js.t) Js.constr) (a) =
  (Js.Unsafe.new_obj constr [|
    Js.Unsafe.inject (a : 'a)
                            |] : 'z Js.t)

let jsnew2 (constr : ('a -> 'b -> 'z Js.t) Js.constr) (a,b) =
  (Js.Unsafe.new_obj constr [|
    Js.Unsafe.inject (a : 'a);
    Js.Unsafe.inject (b : 'b);
                            |] : 'z Js.t)

let jsnew3 (constr : ('a -> 'b -> 'c -> 'z Js.t) Js.constr) (a,b,c) =
  (Js.Unsafe.new_obj constr [|
    Js.Unsafe.inject (a : 'a);
    Js.Unsafe.inject (b : 'b);
    Js.Unsafe.inject (c : 'c);
                            |] : 'z Js.t)

let setIntervalUntilFalse f time =
  let interval_id = ref None in
  let f () =
    if not (f ()) then
      match !interval_id with
          None -> ()
        | Some interval_id ->
          window##clearInterval (interval_id)
  in
  interval_id := Some (window##setInterval (_f f, time))

let setInterval f time =
  let interval_id = window##setInterval (_f f, time) in
  (fun _ -> window##clearInterval (interval_id))

let setTimeout f time =
  let interval_id = window##setTimeout (_f f, time) in
  (fun _ -> window##clearTimeout (interval_id))

let select selection =
  doc##querySelector (_s selection)
    
let select_all selection  =
  doc##querySelectorAll (_s selection)

let from_option opt =  
  match Js.Opt.to_option opt with
  | None ->  assert false
  | Some t -> t
