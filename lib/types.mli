open Core.Std
open Async.Std

module Message : sig
  module Id : sig
    type t
    val to_string : t -> string
  end 

  type t

  val sender : t -> string
  val receivers : t -> string list
  val email : t -> Email_message.Email.t
  val id : t -> Id.t
                  
  val create : 
    sender:string 
    -> receivers:string list 
    -> email:Email_message.Email.t 
    -> t
end
                                                 
module Rule : sig
  module Rewrite : sig
    type t = Message.t -> Message.t list option
  end

  module Route : sig
    type t = Message.t -> (string * int) list option
  end
end

module Commands : sig
  include (module type of Comm)
  val commands : string list
  val to_string : t -> string
  val of_string_opt: string -> t option
end

module Replies : sig
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

  (* CR dlobraico: Change to: 

     type reply = ok Or_error.t

   *)
  type reply = Ok of ok | Bad of not_ok | Really_bad of never_ok
  type t = reply * string

  val to_string : t -> string
  val of_string : string -> t
                              
  val is_last_line : string -> bool
end
