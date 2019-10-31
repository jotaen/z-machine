(ns z-machine.instruction-table)

(defn instruction-table [byte]
  (case byte
    ; 2OP
    ; long form         | variable form
    ; SS | SV | VS | VV | (type in 2nd byte)
    (0x01 0x21 0x41 0x61 0xc1) { :name :je :branch true }
    (0x02 0x22 0x42 0x62 0xc2) { :name :jl :branch true }
    (0x03 0x23 0x43 0x63 0xc3) { :name :jg :branch true }
    (0x04 0x24 0x44 0x64 0xc4) { :name :dec_chk :branch true }
    (0x05 0x25 0x45 0x65 0xc5) { :name :inc_chk :branch true }
    (0x06 0x26 0x46 0x66 0xc6) { :name :jin :branch true }
    (0x07 0x27 0x47 0x67 0xc7) { :name :test :branch true }
    (0x08 0x28 0x48 0x68 0xc8) { :name :or :store true }
    (0x09 0x29 0x49 0x69 0xc9) { :name :and :store true }
    (0x0a 0x2a 0x4a 0x6a 0xca) { :name :test_attr :branch true }
    (0x0b 0x2b 0x4b 0x6b 0xcb) { :name :set_attr }
    (0x0c 0x2c 0x4c 0x6c 0xcc) { :name :clear_attr }
    (0x0d 0x2d 0x4d 0x6d 0xcd) { :name :store }
    (0x0e 0x2e 0x4e 0x6e 0xce) { :name :insert_obj }
    (0x0f 0x2f 0x4f 0x6f 0xcf) { :name :loadw :store true }
    (0x10 0x30 0x50 0x70 0xd0) { :name :loadb :store true }
    (0x11 0x31 0x51 0x71 0xd1) { :name :get_prop :store true }
    (0x12 0x32 0x52 0x72 0xd2) { :name :get_prop_addr :store true }
    (0x13 0x33 0x53 0x73 0xd3) { :name :get_next_prop :store true }
    (0x14 0x34 0x54 0x74 0xd4) { :name :add :store true }
    (0x15 0x35 0x55 0x75 0xd5) { :name :sub :store true }
    (0x16 0x36 0x56 0x76 0xd6) { :name :mul :store true }
    (0x17 0x37 0x57 0x77 0xd7) { :name :div :store true }
    (0x18 0x38 0x58 0x78 0xd8) { :name :mod :store true }
    (0x19 0x39 0x59 0x79 0xd9) { :name :call_2s :store true }
    (0x1a 0x3a 0x5a 0x7a 0xda) { :name :call_2n }
    (0x1b 0x3b 0x5b 0x7b 0xdb) { :name :set_colour }
    (0x1c 0x3c 0x5c 0x7c 0xdc) { :name :throw }

    ; 1OP, all short
    ; L  | S  | V
    (0x80 0x90 0xa0) { :name :jz :branch true }
    (0x81 0x91 0xa1) { :name :get_sibling :branch true :store true }
    (0x82 0x92 0xa2) { :name :get_child :branch true :store true }
    (0x83 0x93 0xa3) { :name :get_parent :store true }
    (0x84 0x94 0xa4) { :name :get_prop_len :store true }
    (0x85 0x95 0xa5) { :name :inc }
    (0x86 0x96 0xa6) { :name :dec }
    (0x87 0x97 0xa7) { :name :print_addr }
    (0x88 0x98 0xa8) { :name :call_1s :store true }
    (0x89 0x99 0xa9) { :name :remove_obj }
    (0x8a 0x9a 0xaa) { :name :print_obj }
    (0x8b 0x9b 0xab) { :name :ret }
    (0x8c 0x9c 0xac) { :name :jump }
    (0x8d 0x9d 0xad) { :name :print_paddr }
    (0x8e 0x9e 0xae) { :name :load :store true }
    (0x8f 0x9f 0xaf) { :name :not :store true }

    ; 0OP, all short
    (0xb0) { :name :rtrue }
    (0xb1) { :name :rfalse }
    (0xb2) { :name :print }
    (0xb3) { :name :print_ret }
    (0xb4) { :name :nop }
    (0xb5) { :name :save :branch true }
    (0xb6) { :name :restore :branch true }
    (0xb7) { :name :restart }
    (0xb8) { :name :ret_popped }
    (0xb9) { :name :pop :store true }
    (0xba) { :name :quit }
    (0xbb) { :name :new_line }
    (0xbc) { :name :show_status }
    (0xbd) { :name :verify :branch true }
    (0xbe) { :name :extended }
    (0xbf) { :name :piracy :branch true }

    ; VAR
    (0xe0) { :name :call :store true }
    (0xe1) { :name :storew }
    (0xe2) { :name :storeb }
    (0xe3) { :name :put_prop }
    (0xe4) { :name :sread :store true }
    (0xe5) { :name :print_char }
    (0xe6) { :name :print_num }
    (0xe7) { :name :random :store true }
    (0xe8) { :name :push }
    (0xe9) { :name :pull :store true }
    (0xea) { :name :split_window }
    (0xeb) { :name :set_window }
    (0xec) { :name :call_vs2 :store true }
    (0xed) { :name :erase_window }
    (0xee) { :name :erase_line }
    (0xef) { :name :set_cursor }
    (0xf0) { :name :get_cursor }
    (0xf1) { :name :set_text_style }
    (0xf2) { :name :buffer_mode }
    (0xf3) { :name :output_stream }
    (0xf4) { :name :input_stream }
    (0xf5) { :name :sound_effect }
    (0xf6) { :name :read_char :store true }
    (0xf7) { :name :scan_table :branch true :store true }
    (0xf8) { :name :not :store true }
    (0xf9) { :name :call_vn }
    (0xfa) { :name :call_vn2 }
    (0xfb) { :name :tokenise }
    (0xfc) { :name :encode_text }
    (0xfd) { :name :copy_table }
    (0xfe) { :name :print_table }
    (0xff) { :name :check_arg_count :branch true }
  ))

(defn instruction-table-extended [byte]
  (case byte
    ; EXT
    (0x00) { :name :save :store true }
    (0x01) { :name :restore :store true }
    (0x02) { :name :log_shift :store true }
    (0x03) { :name :art_shift :store true }
    (0x04) { :name :set_font :store true }
    (0x05) { :name :draw_picture }
    (0x06) { :name :picture_data :branch true }
    (0x07) { :name :erase_picture }
    (0x08) { :name :set_margins }
    (0x09) { :name :save_undo :store true }
    (0x0a) { :name :restore_undo :store true }
    (0x0b) { :name :print_unicode }
    (0x0c) { :name :check_unicode }
    (0x0d) { :name :set_true_colour }
    (0x10) { :name :move_window }
    (0x11) { :name :window_size }
    (0x12) { :name :window_style }
    (0x13) { :name :get_wind_prop :store true }
    (0x14) { :name :scroll_window }
    (0x15) { :name :pop_stack }
    (0x16) { :name :read_mouse }
    (0x17) { :name :mouse_window }
    (0x18) { :name :push_stack :branch true }
    (0x19) { :name :put_wind_prop }
    (0x1a) { :name :print_form }
    (0x1b) { :name :make_menu :branch true }
    (0x1c) { :name :picture_table }
    (0x1d) { :name :buffer_screen :store true }
  ))
