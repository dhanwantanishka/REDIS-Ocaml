open Eio

type resp_value =
  | SimpleString of string
  | Error of string
  | Integer of int
  | BulkString of string option
  | Array of resp_value list option

exception Parse_error of string

let rec parse_resp_value buffer = function
  | '+' -> parse_simple_string buffer
  | '-' -> parse_error buffer
  | ':' -> parse_integer buffer
  | '$' -> parse_bulk_string buffer
  | '*' -> parse_array buffer
  | c -> raise (Parse_error ("Unknown RESP type: " ^ String.make 1 c))

and parse_simple_string buffer = SimpleString (Buf_read.line buffer)
and parse_error buffer = Error (Buf_read.line buffer)
and parse_integer buffer = Integer (buffer |> Buf_read.line |> int_of_string)

and parse_bulk_string buffer =
  let len_line = Buf_read.line buffer in
  let len = int_of_string len_line in
  match len with
  | -1 -> BulkString None
  | 0 ->
    let _ = Buf_read.line buffer in
    BulkString (Some "")
  | l ->
    let content = Buf_read.take l buffer in
    let _ = Buf_read.line buffer in
    BulkString (Some content)

and parse_array buffer =
  let len_line = Buf_read.line buffer in
  let len = int_of_string len_line in
  if len = -1
  then Array None
  else (
    let rec parse_elements acc remaining =
      if remaining = 0
      then List.rev acc
      else (
        let first_char = Buf_read.any_char buffer in
        let element = parse_resp_value buffer first_char in
        parse_elements (element :: acc) (remaining - 1))
    in
    let elements = parse_elements [] len in
    Array (Some elements))
;;

let parse_resp buffer =
  try
    let first_char = Buf_read.any_char buffer in
    parse_resp_value buffer first_char
  with
  | End_of_file -> raise (Parse_error "Empty Input")
;;

let handle_client socket _addr =
  let bufr = Buf_read.of_flow socket ~initial_size:1024 ~max_size:1_000_000 in
  while true do
    let _ = parse_resp bufr in
    Flow.copy_string "+PONG\r\n" socket;
    traceln "socket sent PONG"
  done
;;

let main ~net ~addr =
  Switch.run ~name:"server"
  @@ fun sw ->
  let listening_socekt = Net.listen net ~sw ~reuse_addr:true ~backlog:1000 addr in
  Fiber.fork ~sw (fun () ->
    Net.run_server
      listening_socekt
      handle_client
      ~on_error:(traceln "Error handling connection: %a" Fmt.exn))
;;

let () =
  Eio_posix.run
  @@ fun env -> main ~net:(Stdenv.net env) ~addr:(`Tcp (Net.Ipaddr.V4.loopback, 6379))
;;
