    // Common addr's
    .const start_addr = $801
    .const basic_reentry = $80b
    .const intro_entry = $861
    .const music = $0a00

    // Sine tables
    //  Must be 256-byte aligned
    .const sine_tables = $4000

    .const zoom_plasma_x_offset_low_tab = sine_tables
    .const zoom_plasma_x_offset_high_tab = zoom_plasma_x_offset_low_tab + $100

    .const zoom_plasma_x_interval_low_tab = zoom_plasma_x_offset_high_tab + $100
    .const zoom_plasma_x_interval_high_tab = zoom_plasma_x_interval_low_tab + $100

    .const zoom_plasma_x_move_low_tab = zoom_plasma_x_interval_high_tab + $100
    .const zoom_plasma_x_move_high_tab = zoom_plasma_x_move_low_tab + $100

    .const zoom_plasma_x_flow_tab = zoom_plasma_x_move_high_tab + $100
    .const zoom_plasma_x_flow_low_tab = zoom_plasma_x_flow_tab
    .const zoom_plasma_x_flow_high_tab = zoom_plasma_x_flow_low_tab + $100

    .const zoom_plasma_y_offset_low_tab = zoom_plasma_x_flow_high_tab + $100
    .const zoom_plasma_y_offset_high_tab = zoom_plasma_y_offset_low_tab + $100

    .const zoom_plasma_y_interval_low_tab = zoom_plasma_y_offset_high_tab + $100
    .const zoom_plasma_y_interval_high_tab = zoom_plasma_y_interval_low_tab + $100

    .const zoom_plasma_y_move_low_tab = zoom_plasma_y_interval_high_tab + $100
    .const zoom_plasma_y_move_high_tab = zoom_plasma_y_move_low_tab + $100

    .const zoom_plasma_y_flow_tab = zoom_plasma_y_move_high_tab + $100
    .const zoom_plasma_y_flow_low_tab = zoom_plasma_y_flow_tab
    .const zoom_plasma_y_flow_high_tab = zoom_plasma_y_flow_low_tab + $100

    .const layers_sin_tab_1 = zoom_plasma_y_flow_high_tab + $100
    .const layers_sin_tab_1_low = layers_sin_tab_1
    .const layers_sin_tab_1_high = layers_sin_tab_1_low + $100

    .const layers_sin_tab_2 = layers_sin_tab_1_high + $100
    .const layers_sin_tab_2_low = layers_sin_tab_2
    .const layers_sin_tab_2_high = layers_sin_tab_2_low + $100

    .const sine_boxes_plasma_tab_4 = layers_sin_tab_2_high + $100
    .const sine_boxes_plasma_tab_4_low = sine_boxes_plasma_tab_4
    .const sine_boxes_plasma_tab_4_high = sine_boxes_plasma_tab_4_low + $100

    .const sine_boxes_plasma_tab_3 = sine_boxes_plasma_tab_4_high + $100
    .const sine_boxes_plasma_tab_3_low = sine_boxes_plasma_tab_3
    .const sine_boxes_plasma_tab_3_high = sine_boxes_plasma_tab_3_low + $100

    .const sine_boxes_plasma_tab_2 = sine_boxes_plasma_tab_3_high + $100
    .const sine_boxes_plasma_tab_2_low = sine_boxes_plasma_tab_2
    .const sine_boxes_plasma_tab_2_high = sine_boxes_plasma_tab_2_low + $100

    .const sine_boxes_plasma_tab_1 = sine_boxes_plasma_tab_2_high + $100
    .const sine_boxes_plasma_tab_1_low = sine_boxes_plasma_tab_1
    .const sine_boxes_plasma_tab_1_high = sine_boxes_plasma_tab_1_low + $100

    /*.const sine_boxes_plasma_y_tab = sine_boxes_plasma_tab_1_high + $100
    .const sine_boxes_plasma_y_tab_low = sine_boxes_plasma_y_tab
    .const sine_boxes_plasma_y_tab_high = sine_boxes_plasma_y_tab_low + $100*/
    .const sine_boxes_plasma_y_tab = layers_sin_tab_1
    .const sine_boxes_plasma_y_tab_low = layers_sin_tab_1_low
    .const sine_boxes_plasma_y_tab_high = layers_sin_tab_1_high

    .const sine_boxes_dynamic_base_tab = sine_boxes_plasma_tab_1_high/*sine_boxes_plasma_y_tab_high*/ + $100
    .const sine_boxes_dynamic_base_tab_low = sine_boxes_dynamic_base_tab
    .const sine_boxes_dynamic_base_tab_high = sine_boxes_dynamic_base_tab_low + $100

    .const sine_boxes_sprite_offset_x_tab = sine_boxes_dynamic_base_tab_high + $100
    .const sine_boxes_sprite_offset_x_tab_low = sine_boxes_sprite_offset_x_tab
    .const sine_boxes_sprite_offset_x_tab_high = sine_boxes_sprite_offset_x_tab_low + $100

    .const sine_boxes_sprite_offset_y_tab = sine_boxes_sprite_offset_x_tab_high + $100
    .const sine_boxes_sprite_offset_y_tab_low = sine_boxes_sprite_offset_y_tab
    .const sine_boxes_sprite_offset_y_tab_high = sine_boxes_sprite_offset_y_tab_low + $100

    .const fld_text_fld_y_tab = sine_boxes_sprite_offset_y_tab_high + $100
    .const fld_text_fld_y_tab_low = fld_text_fld_y_tab
    .const fld_text_fld_y_tab_high = fld_text_fld_y_tab_low + $100

    .const sine_tables_end = fld_text_fld_y_tab_high + $100
    .print "sine tables end: " + toHexString(sine_tables_end)

    // Block manipulation
    .const block_manipulation_zp_base = $40

    .const block_read_addr = block_manipulation_zp_base
    .const block_read_addr_low = block_read_addr
    .const block_read_addr_high = block_manipulation_zp_base + 1

    .const block_write_addr = block_manipulation_zp_base + 2
    .const block_write_addr_low = block_write_addr
    .const block_write_addr_high = block_manipulation_zp_base + 3

    .const block_manipulation_interface = $9f2
    .const block_copy_bytes = block_manipulation_interface
    .const block_write_byte = block_manipulation_interface + 3
    .const block_inc_read_addr = block_manipulation_interface + 6
    .const block_inc_write_addr = block_manipulation_interface + 9

    // BG thread dispatch
    .const bg_thread_dispatch = $50

    // Cycle counter
    .const cycle_frames = 22 * 8
    .const cycle_frame_counter = $51
    .const cycle_counter = $52
