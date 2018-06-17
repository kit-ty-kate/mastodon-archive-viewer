open Containers

type create_obj = {
  content : string;
  (* TODO: Use the other fields *)
}

type announce_obj = {
  raw_object : string; (* TODO: To parse later *)
}

type typ =
  | Create of create_obj
  | Announce of announce_obj
  | Unknown

type item = {
  typ : typ;
  published : string; (* TODO: Use some calendar lib *)
}

let create_create_obj l =
  let content =
    match List.Assoc.get_exn ~eq:String.equal "content" l with
    | `String s -> s
    | _ -> assert false
  in
  {content}

let create_create_obj l =
  match List.Assoc.get_exn ~eq:String.equal "object" l with
  | `O l -> create_create_obj l
  | _ -> assert false

let create_announce_obj l =
  match List.Assoc.get_exn ~eq:String.equal "object" l with
  | `String raw_object -> {raw_object}
  | _ -> assert false

let create_item l =
  let typ =
    match List.Assoc.get_exn ~eq:String.equal "type" l with
    | `String "Create" -> Create (create_create_obj l)
    | `String "Announce" -> Announce (create_announce_obj l)
    | `String _ -> Unknown
    | _ -> assert false
  in
  let published =
    match List.Assoc.get_exn ~eq:String.equal "published" l with
    | `String s -> s
    | _ -> assert false
  in
  {typ; published}


let parse_item = function
  | `O l -> create_item l
  | _ -> assert false

let parse_items = function
  | `A l -> List.map parse_item l
  | _ -> assert false

let parse = function
  | `A _ -> assert false
  | `O l -> parse_items (List.Assoc.get_exn ~eq:String.equal "orderedItems" l)

let view_item {typ; published} =
  print_endline "------------------";
  match typ with
  | Create {content} -> Printf.printf "Create at %s: %s\n" published content
  | Announce {raw_object} -> Printf.printf "Announce at %s: %s\n" published raw_object
  | Unknown -> print_endline "Unknown"

let view items =
  List.iter view_item items

let () =
  match Sys.argv with
  | [|_; file|] -> view (parse (IO.with_in file Ezjsonm.from_channel))
  | _ -> assert false
