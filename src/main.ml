open Unix

let process_req _ _ =
  let res = "+PONG\r\n" in
  res

let handle_client client_socket =
  try
    let buffer = Bytes.create 1024 in
    let bytes_read = ref (read client_socket buffer 0 1024) in
    while !bytes_read > 0 do
      let response = process_req buffer bytes_read in
      let _ =
        write client_socket (String.to_bytes response) 0
          (String.length response)
      in
      bytes_read := read client_socket buffer 0 1024
    done
  with
  | Unix_error (ECONNRESET, _, _) -> ()
  | End_of_file -> ()

let run_and_close client_socket =
  let finally () = close client_socket in
  let work () = handle_client client_socket in
  Fun.protect ~finally work

let rec accept_loop server_socket =
  let client_socket, _ = accept server_socket in
  let _ = Thread.create run_and_close client_socket in
  accept_loop server_socket

let () =
  (* You can use print statements as follows for debugging, they'll be visible when running tests. *)
  Printf.eprintf "Logs from your program will appear here!\n";

  (* Create a TCP server socket *)
  let server_socket = socket PF_INET SOCK_STREAM 0 in
  setsockopt server_socket SO_REUSEADDR true;
  bind server_socket (ADDR_INET (inet_addr_of_string "127.0.0.1", 6379));
  listen server_socket 10;

  (* Start handling requests *)
  try accept_loop server_socket
  with e ->
    close server_socket;
    raise e