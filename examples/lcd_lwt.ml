(** An example with Adafruit character LCD, HD44780. *)
open Gpio3
open Lcd
open Lwt

let lcd = {
    columns = 16;
    rows = 2;
    rs = GPIO20;
    en = GPIO16;
    d4 = GPIO19;
    d5 = GPIO5;
    d6 = GPIO11;
    d7 = GPIO10;
}

(** A nicer abstraction *)
module Cursor = struct
  type t = { x: int; y: int; visible: bool; blink: bool; _lcd: mono_lcd }

  (** Creates a new cursor at the position (0,0) *)
  let of_lcd lcd =
    set_position lcd 0 0;
    { x = 0; y = 0; visible = false; blink = false; _lcd = lcd }

  (** Explicitly set the position of the cursor. The arguments will be
     taken modulo the size of the LCD. *)
  let set_position x y cur =
    let col = x mod cur._lcd.columns in
    let row = y mod cur._lcd.rows in
    set_position cur._lcd col row;
    { cur with x = col; y = row }

  (** Whether to display the cursor *)
  let set_visible flag cur =
    let _blinkon = if cur.blink then _lcd_blinkon else _lcd_blinkoff in
    let _cursoron = if flag then _lcd_cursoron else _lcd_cursoroff in
    let displayctrl = _lcd_displayon lor _cursoron lor _blinkon in
    write8_unsafe lcd (displayctrl lor _lcd_displaycontrol);
    { cur with visible = flag }

  (** Whether the cursor is blinking *)
  let set_blink flag cur =
    let _blinkon = if flag then _lcd_blinkon else _lcd_blinkoff in
    let _cursoron = if cur.visible then _lcd_cursoron else _lcd_cursoroff in
    let displayctrl = _lcd_displayon lor _cursoron lor _blinkon in
    write8_unsafe lcd (displayctrl lor _lcd_displaycontrol);
    { cur with blink = flag }

  (** Try writing a character, optionally with wrapping. *)
  let write_char wrapping v cur =
    write8 cur._lcd ~char_mode:true v;
    let col = cur.x + 1 in
    if not wrapping || col < cur._lcd.columns
    then { cur with x = col }
    else let row = cur.y + 1 in
         let col = 0 in
         (* set_position should do the wrapping for us *)
         set_position col row cur

  let write_bytes wrapping bts cur =
    let f cur chr =
      if chr = '\n'
      then set_position 0 (cur.y + 1) cur
      else if chr = '\r'
      then set_position 0 cur.y cur
      else write_char wrapping chr cur in
    Seq.fold_left f cur (Bytes.to_seq bts)

  let write_string ?(wrap=false) str cur = write_bytes wrap (Bytes.of_string str) cur

end

(** A useful combinator for the functions from the [Cursor] module *)
let (|>) (m : Cursor.t) (f : Cursor.t -> 'b) = f m

(** Display two lines without wrapping *)
let display_lines l1 l2 =
  let col_shift = 3 in
  let maxwidth = int_of_float (float_of_int lcd.columns *. 2.5) - col_shift in
  let trunc str =
    let n = min maxwidth (String.length str) in
    String.sub str 0 n
  in
  let open Cursor in
  clear lcd;
  of_lcd lcd
  |> set_visible false
  |> set_blink false
  |> set_position col_shift 0 
  |> write_string (trunc l1) ~wrap:false
  |> set_position col_shift 1
  |> write_string (trunc l2) ~wrap:false

(** Scrolling thread *)

let rec scroll lcd sleep_for : unit Lwt.t =
  shift_left lcd;
  Lwt_unix.sleep sleep_for >>= fun () ->
  scroll lcd sleep_for


(** Networking stuff *)

let create_socket () =
  let open Lwt_unix in
  let sock = socket PF_INET SOCK_STREAM 0 in
  (* let bind_addr = Unix.inet_addr_loopback in *)
  gethostbyname "balthasar.local" >>= fun bind_addr ->
  let bind_addr = bind_addr.h_addr_list.(0) in
  bind sock @@ ADDR_INET(bind_addr, 8080) >>= fun () ->
  listen sock 10;
  return sock

let rec handle_message ic oc () =
  Lwt_io.read_line_opt ic >>= fun line1 ->
  Lwt_io.read_line_opt ic >>= fun line2 ->
  match line1,line2 with
  | Some l1, Some l2 ->
     Lwt_io.printl "Recieved the following message:" >>= fun () ->
     Lwt_io.printl l1 >>= fun () ->
     Lwt_io.printl l2 >>= fun () ->
     Lwt.async (fun () -> Lwt_io.close oc);
     ignore (display_lines l1 l2);
     return ()
  | _,_ -> return ()

let handle_connection conn =
  let fd, _ = conn in
  let ic = Lwt_io.of_fd Lwt_io.Input fd in
  let oc = Lwt_io.of_fd Lwt_io.Output fd in
  Lwt.on_failure (handle_message ic oc ())
    (fun e -> Printf.printf "Error in `handle_message': %s\n" (Printexc.to_string e));
  return ()

let create_server sock =
  let rec serve () =
    Lwt_unix.accept sock >>= handle_connection >>= serve
  in serve

let main () =
  Lwt.async (fun () -> scroll lcd 0.7);
  create_socket () >>= fun sock ->
  create_server sock ()


let _ =
  setup lcd;
  clear lcd;
  ignore (display_lines "Hello, " "world!");
  Lwt_main.run @@ main ()
  
