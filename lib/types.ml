open Core.Std
open Async.Std
module Octet_stream = Email_message.Octet_stream

module Message = struct
  module Id = struct
    type t = string with sexp

    let create sender receivers email =
      let n = Time.hash (Time.now ()) in
      let s = String.hash sender in
      let r = String.hash (List.to_string ~f:String.to_string receivers) in
      List.map ~f:Int.to_string [ n ; s ; r ]
      |> String.concat ~sep:""
    ;;
      
    let to_string = Fn.id
  end 

  type t = 
      { sender : string
      ; receivers : string list 
      ; id : Id.t 
      ; email : Email_message.Email.t 
      } with sexp, fields
  ;;
                   
  let create ~sender ~receivers ~email =
    let id = Id.create sender receivers email in
    Fields.create ~sender ~receivers ~email ~id
  ;;
end
                                                 
module Rule = struct
  module Rewrite = struct
    type t = Message.t -> Message.t list option
  end

  module Route = struct
    type t = Message.t -> (string * int) list option
  end
end

module Commands = struct
  include Comm
  let commands = ["HELO";"MAIL";"FROM";"RCPT";"TO";"DATA";"QUIT";"HELP";"NOOP"]

  let of_string_opt str =
    try Some
      (match
        Lexer.parse_command (Octet_stream.to_lexbuf (Octet_stream.of_string str))
       with
        | Hello s -> Hello (String.lstrip s)
        | Sender s -> Sender (String.lstrip s)
        | Receiver s -> Receiver (String.lstrip s)
        | x -> x)
    with _ -> None

  let to_string = function
  | Hello string -> "HELO " ^ string
  | Sender string -> "MAIL FROM: " ^ string
  | Receiver string -> "RCPT TO: " ^ string
  | Data -> "DATA"
  | Quit -> "QUIT"
  | Help -> "HELP"
  | Noop -> "NOOP"
end

module Replies = struct
  type ok =
  | System_status
  | Help
  | Service_ready
  | Closing_connection
  | Ok_completed
  | Will_forward
  | Will_attempt
  | Start_mail_input

  type not_ok =
  | Not_available
  | Mailbox_unavailable_400
  | Local_error
  | Insufficient_storage
  | Unable_to_accommodate

  type never_ok =
  | Command_not_recognized
  | Syntax_error
  | Command_not_implemented
  | Bad_sequence_of_commands
  | Parameter_not_implemented
  | Mailbox_unavailable_500
  | User_not_local
  | Exceeded_storage_allocation
  | Mailbox_name_not_allowed
  | Trasaction_failed
  | From_to_parameters_bad

  type reply = Ok of ok | Bad of not_ok | Really_bad of never_ok
  type t = reply * string
  exception Unknown_response_code of int
  exception Unexpected_character of char
  let my_name = Unix.gethostname ()

  let to_string = fun (reply,msg) -> match reply with
  | Ok r -> begin
    match r with
    | System_status -> "211"
    | Help ->
      "214-Commands supported:\n214 "
      ^ (String.concat ~sep:" " Commands.commands)
    | Service_ready ->
      "220 "^ my_name ^ " SMTP JS_SMTP v0.001 "
      ^ (Time.to_string_abs (Time.now ()))
    | Closing_connection -> "221 " ^ my_name ^ " closing connection"
    | Ok_completed -> "250 Ok: " ^ msg
    | Will_forward -> "251"
    | Will_attempt -> "252"
    | Start_mail_input -> "354 Enter message, ending with \".\" on a line by itself"
    end
  | Bad r -> begin
    match r with
    | Not_available -> "421"
    | Mailbox_unavailable_400 -> "450"
    | Local_error -> "451"
    | Insufficient_storage -> "452"
    | Unable_to_accommodate -> "455"
    end
  | Really_bad r -> begin
    match r with
    | Command_not_recognized -> "500 unrecognized command"
    | Syntax_error -> "501 Syntactically invalid command"
    | Command_not_implemented -> "502 Command not implemented"
    | Bad_sequence_of_commands -> "503 sender already given"
    | Parameter_not_implemented -> "504 Command parameter not implemented"
    | Mailbox_unavailable_500 -> "550 Mailbox unavailable"
    | User_not_local -> "551 User not local"
    | Exceeded_storage_allocation -> "552 Exceeded storage allocation"
    | Mailbox_name_not_allowed -> "553 Mailbox name not allowed"
    | Trasaction_failed -> "554 Trasaction failed"
    | From_to_parameters_bad -> "555"
    end

  let is_last_line str =
    if Polymorphic_compare.equal 3 (String.length str) then true
    else match String.get str 3 with
    | ' ' -> true
    | '-' -> false
    | c -> raise (Unexpected_character c)

  let of_string str =
    ( begin
      match Int.of_string (String.sub ~pos:0 ~len:3 str) with
      | 211 -> Ok System_status
      | 214 -> Ok Help
      | 220 -> Ok Service_ready
      | 221 -> Ok Closing_connection
      | 250 -> Ok Ok_completed
      | 251 -> Ok Will_forward
      | 252 -> Ok Will_attempt
      | 354 -> Ok Start_mail_input

      | 421 -> Bad Not_available
      | 450 -> Bad Mailbox_unavailable_400
      | 451 -> Bad Local_error
      | 452 -> Bad Insufficient_storage
      | 455 -> Bad Unable_to_accommodate

      | 500 -> Really_bad Command_not_recognized
      | 501 -> Really_bad Syntax_error
      | 502 -> Really_bad Command_not_implemented
      | 503 -> Really_bad Bad_sequence_of_commands
      | 504 -> Really_bad Parameter_not_implemented
      | 550 -> Really_bad Mailbox_unavailable_500
      | 551 -> Really_bad User_not_local
      | 552 -> Really_bad Exceeded_storage_allocation
      | 553 -> Really_bad Mailbox_name_not_allowed
      | 554 -> Really_bad Trasaction_failed
      | 555 -> Really_bad From_to_parameters_bad
      | x -> raise (Unknown_response_code x)
      end
    , str)
end

