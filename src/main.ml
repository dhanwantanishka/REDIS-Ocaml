open Unix

let write_str fd s =
  let buf = Bytes.of_string s in
  let pos = 0 in
  let len = Bytes.length buf in
  write fd buf pos len
;;

let read_str fd =
  let buf_len = 4096 in
  let buf = Bytes.create buf_len in
  let len = read fd buf 0 buf_len in
  Bytes.sub_string buf 0 len
;;

let () =
  let server_socket = socket PF_INET SOCK_STREAM 0 in
  setsockopt server_socket SO_REUSEADDR true;
  bind server_socket (ADDR_INET (inet_addr_of_string "127.0.0.1", 6379));
  listen server_socket 1;
  let client_socket, _ = accept server_socket in
  let rec handle_client () =
    try
      let msg = read_str client_socket in
      if String.length msg = 0
      then (
        close client_socket;
        close server_socket)
      else (
        ignore (write_str client_socket "+PONG\r\n");
        handle_client ())
    with
    | Unix_error (ECONNRESET, _, _) | End_of_file ->
      close client_socket;
      close server_socket
  in
  handle_client ()
;;