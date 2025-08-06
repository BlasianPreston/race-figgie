open Server

let command_group =
  Command.group
    ~summary:"Race Figgie"
    [ "start-server", start_server_command ]
;;

let () = Command_unix.run command_group