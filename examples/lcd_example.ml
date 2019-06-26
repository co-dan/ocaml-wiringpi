(** An example with Adafruit character LCD, HD44780. *)
open Gpio3
open Lcd

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

let _ =
  setup lcd;
  clear lcd;
  set_position lcd 0 0;
  write_bytes lcd (Bytes.of_string "Hello, world.")
