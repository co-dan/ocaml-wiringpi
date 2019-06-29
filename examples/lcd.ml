(** An example with Adafruit character LCD, HD44780. *)
(* The documentation: https://cdn-shop.adafruit.com/datasheets/HD44780.pdf *)
open Gpio3

(** Parameters *)
type mono_lcd = {
    columns: int;
    rows: int;
    rs: pin;
    en: pin;
    d4: pin;
    d5: pin;
    d6: pin;
    d7: pin
}

(** MAGIC NUMBERS *)
(* Commands *)
let _lcd_cleardisplay        = (0x01)
let _lcd_returnhome          = (0x02)
let _lcd_entrymodeset        = (0x04)
let _lcd_displaycontrol      = (0x08)
let _lcd_cursorshift         = (0x10)
let _lcd_functionset         = (0x20)
let _lcd_setcgramaddr        = (0x40)
let _lcd_setddramaddr        = (0x80)

(* Entry flags *)
let _lcd_entryleft           = (0x02)
let _lcd_entryshiftdecrement = (0x00)

(* Control flags *)
let _lcd_displayon           = (0x04)
let _lcd_cursoron            = (0x02)
let _lcd_cursoroff           = (0x00)
let _lcd_blinkon             = (0x01)
let _lcd_blinkoff            = (0x00)

(* Move flags *)
let _lcd_displaymove         = (0x08)
let _lcd_moveright           = (0x04)
let _lcd_moveleft            = (0x00)

(* Function set flags *)
let _lcd_4bitmode            = (0x00)
let _lcd_2line               = (0x08)
let _lcd_1line               = (0x00)
let _lcd_5x8dots             = (0x00)

(* Offset for up to 4 rows. *)
let _lcd_row_offsets = [|0x00; 0x40; 0x14; 0x54|]

let pulse_enable lcd =
  digital_write lcd.en LOW;
  (* 1 microsec pause *)
  Unix.sleepf(0.0000001);
  digital_write lcd.en HIGH;
  Unix.sleepf(0.0000001);
  digital_write lcd.en LOW;
  Unix.sleepf(0.00001)

(** write 8 bits of data *)
let write8 lcd ?(char_mode=false) value =
  (* one ms delay to prevent writing too quickly *)
  Unix.sleepf(0.001);
  let char_mode_value = if char_mode then HIGH else LOW in
  digital_write lcd.rs char_mode_value;
  let val_of_int i = if i > 0 then HIGH else LOW in
  let ival = Char.code value in
  (* write the UPPER 4 bits (in reverse order) *)
  digital_write lcd.d4 (val_of_int ((ival lsr 4) land 1));
  digital_write lcd.d5 (val_of_int ((ival lsr 5) land 1));
  digital_write lcd.d6 (val_of_int ((ival lsr 6) land 1));
  digital_write lcd.d7 (val_of_int ((ival lsr 7) land 1));
  pulse_enable lcd;
  (* write the LOWER 4 bits *)
  digital_write lcd.d4 (val_of_int (ival land 1));
  digital_write lcd.d5 (val_of_int ((ival lsr 1) land 1));
  digital_write lcd.d6 (val_of_int ((ival lsr 2) land 1));
  digital_write lcd.d7 (val_of_int ((ival lsr 3) land 1));
  pulse_enable lcd

(** same as [write8] but write an int (must be within the range) *)
let write8_unsafe lcd ?(char_mode=false) ival =
  write8 lcd ~char_mode (Char.unsafe_chr ival)

let setup lcd =
  Gpio3.setup ();
  pin_mode lcd.rs OUT;
  pin_mode lcd.en OUT;
  pin_mode lcd.d4 OUT;
  pin_mode lcd.d5 OUT;
  pin_mode lcd.d6 OUT;
  pin_mode lcd.d7 OUT;
  (* send the 4-bit initialization sequence: 0011, 0011, 0011, 0010.
     see Figure 24 in the docs. *)
  write8_unsafe lcd 0x33;
  write8_unsafe lcd 0x32;
  (* set up some stuff *)
  let displayctrl = _lcd_displayon lor _lcd_cursoroff lor _lcd_blinkoff in
  let displayfn = _lcd_4bitmode lor _lcd_2line lor _lcd_5x8dots in
  let displaymode = _lcd_entryleft lor _lcd_entryshiftdecrement in
  write8_unsafe lcd (displayctrl lor _lcd_displaycontrol);
  write8_unsafe lcd (displayfn lor _lcd_functionset);
  write8_unsafe lcd (displaymode lor _lcd_entrymodeset);
  Unix.sleepf(0.003)

let clear lcd =
  write8_unsafe lcd _lcd_cleardisplay;
  Unix.sleepf(0.003)

let shift_left lcd =
  write8_unsafe lcd (_lcd_cursorshift lor _lcd_displaymove lor _lcd_moveleft)

let shift_right lcd =
  write8_unsafe lcd (_lcd_cursorshift lor _lcd_displaymove lor _lcd_moveright)

let set_position lcd x y =
  let c = x mod lcd.columns in
  let r = y mod lcd.rows in
  if (c < 0 || r < 0) then failwith "Lcd.set_position: Negative row or column";
  write8_unsafe lcd (_lcd_setddramaddr lor (c + _lcd_row_offsets.(r)))

let write_bytes lcd bts =
  Bytes.iter (fun c -> write8 lcd c ~char_mode:true) bts
