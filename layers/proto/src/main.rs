extern crate minifb;
extern crate time;
extern crate image;

use minifb::{Key, Scale, WindowOptions, Window};

use image::{GenericImage, Pixel};

use std::collections::{BTreeMap, BTreeSet};
use std::f64::consts::PI;
use std::fs::File;
use std::io::Write;

fn main() {
    const CHARS_WIDTH: usize = 40;
    const CHARS_HEIGHT: usize = 24;

    const WIDTH: usize = CHARS_WIDTH * 8;
    const HEIGHT: usize = CHARS_HEIGHT * 8;

    let mut buffer: Box<[u32]> = vec![0; WIDTH * HEIGHT].into_boxed_slice();

    let mut window = Window::new("layers proto", WIDTH, HEIGHT, WindowOptions {
        borderless: false,
        title: true,
        resize: false,
        scale: Scale::X2
    }).unwrap_or_else(|e| {
        panic!("{}", e);
    });

    let colors = [
        0x000000,
        0xffffff,
        0x68372b,
        0x70a4b2,
        0x643d86,
        0x588d43,
        0x352879,
        0xb8c76f,
        0x6f4f25,
        0x433900,
        0x9a6759,
        0x444444,
        0x6c6c6c,
        0x9ad284,
        0x6c5eb5,
        0x959595
    ];

    let font_image = image::open("data/font.png").unwrap();

    let mut scroll_text = String::new();
    scroll_text.push_str("code:   ");
    scroll_text.push_str("  ferris");
    scroll_text.push_str("gfx:    ");
    scroll_text.push_str("  ferris");
    scroll_text.push_str("  hobble");
    scroll_text.push_str("music:  ");
    scroll_text.push_str(" hoffman");
    scroll_text.push_str("sid     ");
    scroll_text.push_str("driver: ");
    scroll_text.push_str("    4mat");
    scroll_text.push_str("        ");

    /*scroll_text.push_str(" greets ");
    scroll_text.push_str("   to   ");
    scroll_text.push_str("perform-");
    scroll_text.push_str("   ers! ");
    scroll_text.push_str("for this");
    scroll_text.push_str(" lovely ");
    scroll_text.push_str("original");
    scroll_text.push_str(" effect ");
    scroll_text.push_str("        ");
    scroll_text.push_str("but we  ");
    scroll_text.push_str("  did it");
    scroll_text.push_str("in a 4k!");*/

    scroll_text.push_str(" greets ");
    scroll_text.push_str(" to all ");
    scroll_text.push_str(" of you ");
    /*scroll_text.push_str(" of our ");
    scroll_text.push_str("journey!");
    scroll_text.push_str("        ");
    scroll_text.push_str("thanks  ");
    scroll_text.push_str("     for");
    scroll_text.push_str("watching");*/
    scroll_text.push_str("        ");
    scroll_text.push_str("coma out");
    //scroll_text.push_str("  out!  ");
    // Extra spaces on the end in case we read too far (a bit easier to hack it this way than to special case in the rendering code)
    scroll_text.push_str("        ");
    scroll_text.push_str("        ");
    scroll_text.push_str("        ");
    scroll_text.push_str("        ");
    scroll_text.push_str("        ");
    scroll_text.push_str("        ");
    scroll_text.push_str("        ");
    scroll_text.push_str("        ");

    // Strip unused chars and pack scroll text and font data
    let mut used_font_indices = BTreeSet::new();
    let mut scroll_text_original_font_indices = Vec::new();
    for c in scroll_text.chars() {
        let font_index = if c == ' ' {
            0
        } else if c == '!' {
            27
        } else if c == ':' {
            28
        } else if c == '4' {
            29
        } else if c == '-' {
            30
        } else {
            (c as i32) - ('a' as i32) + 1
        };

        used_font_indices.insert(font_index);
        scroll_text_original_font_indices.push(font_index);
    }
    println!("Used chars: {}", used_font_indices.len());

    let mut packed_font = Vec::new();
    let mut original_font_index_to_packed_font_index = BTreeMap::new();
    for (new_index, original_index) in used_font_indices.into_iter().enumerate() {
        let font_image_y = original_index * 6;
        for char_y in 0..5 {
            let mut b = 0;
            for char_x in 0..3 {
                b >>= 1;
                if font_image.get_pixel(char_x, (font_image_y + char_y) as _).to_rgb().data[0] != 0 {
                    b |= 0x04;
                }
            }
            packed_font.push(b);
        }

        original_font_index_to_packed_font_index.insert(original_index, (new_index * 5) as u8);
    }
    println!("Packed font size: {} bytes", packed_font.len());
    {
        let mut file = File::create("../font.bin").unwrap();
        file.write(&packed_font).unwrap();
    }

    let mut packed_scroll_text = Vec::new();
    for original_index in scroll_text_original_font_indices.into_iter() {
        let new_index = original_font_index_to_packed_font_index[&original_index];
        packed_scroll_text.push(new_index);
    }
    println!("Packed scroll text size: {} bytes", packed_scroll_text.len());
    {
        let mut file = File::create("../scroll-text.bin").unwrap();
        file.write(&packed_scroll_text).unwrap();
    }

    let mut vic_bank: Vec<u8> = vec![0; 16384];

    const SCREEN_MEM_OFFSET_1: usize = 0x2c00;
    const SCREEN_MEM_OFFSET_2: usize = 0x3400;
    const SPRITE_POINTER_OFFSET_1: usize = SCREEN_MEM_OFFSET_1 + 0x03f8;
    const SPRITE_POINTER_OFFSET_2: usize = SCREEN_MEM_OFFSET_2 + 0x03f8;

    let mut screen_mem_index = 0;

    let mut scroll_text_offset = 0;
    let mut scroll_text_offset_two_frame_toggle_enable = true;

    let scroll_text_colors = [
        0x06,
        0x0b,
        0x04,
        0x0c,
        0x0e,
        0x03,
        0x0d,
        0x01,
        0x07,
        0x03,
        0x0e,
        0x05,
        0x04,
        0x02,
        0x06,
        0x09,
    ];
    {
        let mut file = File::create("../scroll-text-colors.bin").unwrap();
        file.write(&scroll_text_colors).unwrap();
    }

    // Since we have 5 rows of 8 sprites to display, we'll use the last 0x200 bytes of each of the first 5 2kb blocks in the
    //  bank to store the data. When we shift rows, we'll increment our pointer values so they point to the next 2kb block,
    //  wrapping accordingly.
    const SPRITE_DATA_BLOCK_OFFSET: usize = 0x0600;

    let mut scroll_text_block = 0;

    let mut scroll_text_text_offset = 0;

    let mut scroll_text_color_offset = 12;
    let mut scroll_text_color_offset_update_enable = false;
    let mut scroll_text_color_mix_toggle = false;
    let mut scroll_text_color_mix_toggle_enable = false;

    let mut screen_mem_pattern_load_write_offset = SCREEN_MEM_OFFSET_1;
    let mut screen_mem_pattern_load_pattern_index = 2;
    let mut screen_mem_pattern_load_y = 0;

    fn screen_mem_pattern_load_line(vic_bank: &mut Vec<u8>, write_offset: &mut usize, pattern_index: u32, y: &mut usize) {
        if *y >= CHARS_HEIGHT + 1 {
            return;
        }

        for x in 0..CHARS_WIDTH {
            vic_bank[*write_offset + x] = ((*y as u8) & 0x03) * 4 + ((x as u8) & 0x03) + ((pattern_index as u8) << 4);
        }

        *write_offset += CHARS_WIDTH;
        *y += 1;
    }

    // Initially load large slash pattern (looks great when separating initially)
    //  Call line fill multiple times to ensure it's finished during load time
    while screen_mem_pattern_load_y < CHARS_HEIGHT + 1 {
        screen_mem_pattern_load_line(&mut vic_bank, &mut screen_mem_pattern_load_write_offset, screen_mem_pattern_load_pattern_index, &mut screen_mem_pattern_load_y);
    }

    // Dispatch next pattern to load
    screen_mem_pattern_load_write_offset = SCREEN_MEM_OFFSET_2;
    screen_mem_pattern_load_pattern_index = 4;
    screen_mem_pattern_load_y = 0;

    for char_set_index in 0..8 {
        let mut char_set_addr = char_set_index * 0x800;

        let char_set_index_2 = char_set_index * 2;
        let char_set_index_3 = char_set_index * 3;
        let char_set_index_4 = char_set_index * 4;

        // Small slash pattern (chars 0-15)
        for _char_y in 0..4 {
            for _char_x in 0..4 {
                for pixel_y in 0..8 {
                    let mut c = 0;
                    for pixel_x in 0..8 {
                        let pixel = (((pixel_x + (7 - pixel_y + char_set_index_2)) >> 2) & 0x01) as u8;
                        c <<= 1;
                        c |= pixel;
                    }
                    vic_bank[char_set_addr as usize] = c;
                    char_set_addr += 1;
                }
            }
        }

        // Checker shift thing (chars 16-31)
        for char_y in 0..4 {
            for char_x in 0..4 {
                for pixel_y in 0..8 {
                    let mut c = 0;
                    for _ in 0..8 {
                        let pixel = (((((char_x + char_y) & 0x01) * 8 + pixel_y - char_set_index_4) >> 4) & 0x01) as u8;
                        c <<= 1;
                        c |= pixel;
                    }
                    vic_bank[char_set_addr as usize] = c;
                    char_set_addr += 1;
                }
            }
        }

        // Large slash pattern (chars 32-47)
        for char_y in 0..4 {
            for char_x in 0..4 {
                for pixel_y in 0..8 {
                    let mut c = 0;
                    for pixel_x in 0..8 {
                        let pixel = ((((char_x + char_y) * 8 + pixel_x + pixel_y + char_set_index_3) >> 4) & 0x01) as u8;
                        c <<= 1;
                        c |= pixel;
                    }
                    vic_bank[char_set_addr as usize] = c;
                    char_set_addr += 1;
                }
            }
        }

        // Vertical bars (chars 48-63)
        for _char_y in 0..4 {
            for char_x in 0..4 {
                for _pixel_y in 0..8 {
                    let mut c = 0;
                    for pixel_x in 0..8 {
                        let pixel = (((char_x * 8 + pixel_x - char_set_index_4) >> 4) & 0x01) as u8;
                        c <<= 1;
                        c |= pixel;
                    }
                    vic_bank[char_set_addr as usize] = c;
                    char_set_addr += 1;
                }
            }
        }

        // Static alternating pattern (chars 64-79)
        for _char_y in 0..4 {
            for _char_x in 0..4 {
                for pixel_y in 0..8 {
                    let mut c = 0;
                    for pixel_x in 0..8 {
                        let pixel = ((pixel_x + pixel_y - char_set_index) & 0x01) as u8;
                        c <<= 1;
                        c |= pixel;
                    }
                    vic_bank[char_set_addr as usize] = c;
                    char_set_addr += 1;
                }
            }
        }
    }

    let mut char_set_index = 0;

    // Start with all black color mem
    let mut color_mem = vec![0; CHARS_WIDTH * (CHARS_HEIGHT + 1)];

    // Generate color layer picture
    //  Colors indices will range from 0-15, where 0-7 represents the palette, and 8-15 represents the reverse palette, so it
    //  forms a nice, looping gradient. We first generate the picture as a list of indices, then apply the palette flipping,
    //  then fade some parts of the pattern, and finally replace the indices with actual VIC palette indices.
    let mut color_layer_picture = Vec::new();

    let color_layer_palette = [
        0x00,
        0x06,
        0x0b,
        0x04,
        0x0e,
        0x03,
        0x0d,
        0x01,
        0x01,
        0x0d,
        0x03,
        0x0e,
        0x04,
        0x0b,
        0x06,
        0x00,
    ];
    {
        let mut file = File::create("../color-layer-palette.bin").unwrap();
        file.write(&color_layer_palette).unwrap();
    }
    for x in color_layer_picture.iter_mut() {
        *x = color_layer_palette[*x as usize];
    }

    let mut original_sin_tab = Vec::new();
    for i in 0..256 {
        let a = (i as f64) / 256.0 * 2.0 * PI;
        let s = a.sin() * 255.0;
        let v = s as i16;
        original_sin_tab.push(v);
    }

    let mut shifted_sin_tab_1 = Vec::new();
    for i in 0..256 {
        let v = (original_sin_tab[i] >> 1) as u8;
        shifted_sin_tab_1.push(v);
    }
    {
        let mut file = File::create("../sin-tab-1.bin").unwrap();
        file.write(&shifted_sin_tab_1).unwrap();
    }

    let mut shifted_sin_tab_2 = Vec::new();
    for i in 0..256 {
        let v = (original_sin_tab[i] >> 5) as u8;
        shifted_sin_tab_2.push(v);
    }
    {
        let mut file = File::create("../sin-tab-2.bin").unwrap();
        file.write(&shifted_sin_tab_2).unwrap();
    }

    // Start with 12 black char lines (2 char rows + spaces between)
    for _ in 0..12 {
        for _ in 0..CHARS_WIDTH {
            color_layer_picture.push(0);
        }
    }
    // Plasma
    for y in 0..CHARS_HEIGHT * 2 {
        for x in 0..CHARS_WIDTH {
            /*let v = (((
                ((x as f64) / 50.0 * 2.0 * PI).sin() * PI +
                ((y as f64 + (x as f64) / 3.0) / 50.0 * 2.0 * PI).sin() * PI
            ).sin() * 7.0).round() as u8) + 8;*/
            let a = shifted_sin_tab_1[((x * 5) as u8) as usize] + shifted_sin_tab_1[((y * 5 + x) as u8) as usize];
            let v = shifted_sin_tab_2[a as usize] + 8;
            color_layer_picture.push(color_layer_palette[v as usize]);
        }
    }
    // Diagonal lines
    for y in 0..CHARS_HEIGHT {
        for x in 0..CHARS_WIDTH {
            color_layer_picture.push(color_layer_palette[(((x + y) & 0x0f) as u8) as usize]);
        }
    }
    // End with a full black screen, plus a couple buffer lines
    for _ in 0..CHARS_HEIGHT + 2 {
        for _ in 0..CHARS_WIDTH {
            color_layer_picture.push(0);
        }
    }

    // Draw in first two text lines over first 12 char screen lines
    //  This needs to happen after we've replaced the color layer indices with palette indices, since we're reading from the scroll text's palette
    for line in 0..2 {
        for i in 0..8 {
            let font_char_offset = packed_scroll_text[line * 8 + i] as usize;

            for char_y in 0..5 {
                let font_byte = packed_font[font_char_offset + char_y];
                let color = scroll_text_colors[((line + 4) * 5 + char_y + (scroll_text_color_offset as usize)) & 0x1f];

                for char_x in 0..3 {
                    if ((font_byte >> char_x) & 0x01) != 0 {
                        color_layer_picture[(line * 6 + char_y) * CHARS_WIDTH + i * 5 + char_x + 1] = color;
                    }
                }
            }
        }
    }

    let mut color_layer_picture_offset = 0;

    let mut scroll_y = 7;

    let mut two_frame_toggle = false;

    let mut frame_count = 0;

    let start_time = time::precise_time_s();
    let mut last_frame_index = 0;

    while window.is_open() && !window.is_key_down(Key::Escape) {
        let time = time::precise_time_s() - start_time;

        // Simulate 50fps by quantizing to 20ms intervals
        let frame_index = (time / 0.020) as i32;

        for _ in 0..frame_index - last_frame_index {
            //println!("Frame count: {}", frame_count);

            // Scripting
            if frame_count == 130 {
                scroll_text_offset_two_frame_toggle_enable = false;
            }

            if frame_count == 280 {
                scroll_text_color_mix_toggle_enable = true;
                scroll_text_color_offset_update_enable = true;
            }

            if frame_count == 330 {
                screen_mem_index = 1;

                screen_mem_pattern_load_write_offset = SCREEN_MEM_OFFSET_1;
                screen_mem_pattern_load_pattern_index = 1;
                screen_mem_pattern_load_y = 0;
            }

            if frame_count == 580 {
                screen_mem_index = 0;

                screen_mem_pattern_load_write_offset = SCREEN_MEM_OFFSET_2;
                screen_mem_pattern_load_pattern_index = 3;
                screen_mem_pattern_load_y = 0;
            }

            if frame_count == 680 {
                screen_mem_index = 1;

                screen_mem_pattern_load_write_offset = SCREEN_MEM_OFFSET_1;
                screen_mem_pattern_load_pattern_index = 0;
                screen_mem_pattern_load_y = 0;
            }

            if frame_count == 760 {
                scroll_text_offset_two_frame_toggle_enable = true;
            }

            if frame_count == 930 {
                screen_mem_index = 0;

                screen_mem_pattern_load_write_offset = SCREEN_MEM_OFFSET_2;
                screen_mem_pattern_load_pattern_index = 4;
                screen_mem_pattern_load_y = 0;
            }

            if frame_count == 1000 {
                scroll_text_offset_two_frame_toggle_enable = false;
            }

            if frame_count == 1200 {
                screen_mem_index = 1;

                screen_mem_pattern_load_write_offset = SCREEN_MEM_OFFSET_1;
                screen_mem_pattern_load_pattern_index = 2;
                screen_mem_pattern_load_y = 0;
            }

            if frame_count == 1280 {
                scroll_text_offset_two_frame_toggle_enable = true;
            }

            if frame_count == 1380 {
                screen_mem_index = 0;

                screen_mem_pattern_load_write_offset = SCREEN_MEM_OFFSET_2;
                screen_mem_pattern_load_pattern_index = 1;
                screen_mem_pattern_load_y = 0;
            }

            if frame_count == 1480 {
                screen_mem_index = 1;
            }

            if frame_count == 1500 {
                scroll_text_offset_two_frame_toggle_enable = false;
            }

            if frame_count == 1726 {
                // We're done!
                return;
            }

            // Update
            screen_mem_pattern_load_line(&mut vic_bank, &mut screen_mem_pattern_load_write_offset, screen_mem_pattern_load_pattern_index, &mut screen_mem_pattern_load_y);

            if !two_frame_toggle {
                scroll_y = (scroll_y - 1) & 0x07;
                char_set_index = (char_set_index + 1) & 0x07;

                if scroll_y == 6 {
                    // Load next color row
                    for i in 0..CHARS_WIDTH {
                        color_mem[CHARS_WIDTH * CHARS_HEIGHT + i] = color_layer_picture[color_layer_picture_offset];
                        color_layer_picture_offset += 1;
                    }
                }

                if scroll_y == 7 {
                    // Shift color mem upwards one row
                    for y in 0..CHARS_HEIGHT {
                        for x in 0..CHARS_WIDTH {
                            color_mem[y * CHARS_WIDTH + x] = color_mem[(y + 1) * CHARS_WIDTH + x];
                        }
                    }
                }

                if scroll_text_color_offset_update_enable {
                    scroll_text_color_offset -= 1;
                }
            }

            if !scroll_text_offset_two_frame_toggle_enable || !two_frame_toggle {
                if scroll_text_offset < 8 * 5 && (scroll_text_offset & 1) == 0 {
                    // Progressively expand next row of scroll text data into last sprite data row
                    let mut scroll_text_expand_block = scroll_text_block + 4;
                    if scroll_text_expand_block >= 5 {
                        scroll_text_expand_block -= 5;
                    }

                    let sprite_y = scroll_text_offset / 2;
                    let mut expand_offset = ((0x0800 * scroll_text_expand_block) as usize) + SPRITE_DATA_BLOCK_OFFSET + ((sprite_y * 3) as usize);

                    for i in 0..8 {
                        let char_y = sprite_y / 4;
                        let mut font_byte = packed_font[(char_y + packed_scroll_text[(scroll_text_text_offset as usize) + i]) as usize];

                        for char_x in 0..3 {
                            let b = if (font_byte & 0x01) != 0 { 0xff } else { 0x00 };
                            vic_bank[expand_offset + char_x] = b;
                            font_byte >>= 1;
                        }

                        expand_offset += 0x40;
                    }
                }

                scroll_text_offset += 1;
                if scroll_text_offset == 8 * 6 {
                    scroll_text_offset = 0;

                    // Shift rows
                    scroll_text_block += 1;

                    scroll_text_text_offset += 8;

                    if scroll_text_block >= 5 {
                        scroll_text_block = 0;
                    }
                    scroll_text_color_offset += 5;
                }
            }

            two_frame_toggle = !two_frame_toggle;

            if scroll_text_color_mix_toggle_enable {
                scroll_text_color_mix_toggle = !scroll_text_color_mix_toggle;
            }

            frame_count += 1;

            // Clear screen
            for p in buffer.iter_mut() {
                *p = 0;
            }

            // Draw sprite layer
            let sprite_pointer_offset = if screen_mem_index == 0 {
                SPRITE_POINTER_OFFSET_1
            } else {
                SPRITE_POINTER_OFFSET_2
            };
            let mut scroll_text_draw_block = scroll_text_block;
            for sprite_row in 0..5 {
                for sprite_index in 0..8 {
                    // Set sprite pointers for this text row
                    //  In the real impl, we'll set the pointers for the first row before the screen is drawn, and we'll update the
                    //  pointers for the other rows with raster interrupts as the screen is being drawn
                    for i in 0..8 {
                        vic_bank[sprite_pointer_offset + i] = ((scroll_text_draw_block * 32) as u8) + (((SPRITE_DATA_BLOCK_OFFSET as i32) / 0x40) as u8) + (i as u8);
                    }

                    // Sprites will be normal width, expanded height
                    let sprite_data_offset = (vic_bank[sprite_pointer_offset + (sprite_index as usize)] as i32) * 0x40;

                    for sprite_y in 0..42 {
                        let sprite_data_row_offset = sprite_data_offset + (sprite_y / 2) * 3;

                        for sprite_x in 0..24 {
                            let sprite_data_byte_offset = sprite_data_row_offset + sprite_x / 8;
                            let sprite_data_byte = vic_bank[sprite_data_byte_offset as usize];
                            let sprite_data_bit_index = 7 - (sprite_x & 0x07);

                            let p = (sprite_data_byte >> sprite_data_bit_index) & 0x01;
                            if p != 0 {
                                let unscaled_x = 1 + sprite_index * 5;
                                let scaled_x = unscaled_x * 8;
                                let unscaled_y = sprite_row * 6;
                                let scaled_y = unscaled_y * 8;
                                let pixel_y = scaled_y + sprite_y - (scroll_text_offset as i32);
                                if pixel_y < 0 || pixel_y >= (HEIGHT as i32) {
                                    continue;
                                }

                                let color_index = (sprite_row * 5 + (sprite_y / 8) + scroll_text_color_offset) & 0x1f;
                                buffer[((pixel_y * (WIDTH as i32)) + (scaled_x + sprite_x)) as usize] = colors[if !scroll_text_color_mix_toggle {
                                    scroll_text_colors[color_index as usize]
                                } else {
                                    scroll_text_colors[((color_index + 1) & 0x1f) as usize]
                                } as usize];
                            }
                        }
                    }
                }

                scroll_text_draw_block += 1;
                if scroll_text_draw_block >= 5 {
                    scroll_text_draw_block = 0;
                }
            }

            // Draw color layer
            for row in 0..(CHARS_HEIGHT + 1) as i32 {
                for col in 0..CHARS_WIDTH as i32 {
                    let char_mem_index = (row * (CHARS_WIDTH) as i32 + col) as usize;
                    let char_index = vic_bank[if screen_mem_index == 0 { SCREEN_MEM_OFFSET_1 } else { SCREEN_MEM_OFFSET_2 } + char_mem_index];
                    let color_index = color_mem[char_mem_index];
                    let color = colors[color_index as usize];
                    for char_y in 0..8 {
                        for char_x in 0..8 {
                            let pixel_x = col * 8 + char_x;
                            let pixel_y = row * 8 + char_y;
                            let draw_y = pixel_y - 7 + scroll_y;
                            if draw_y >= 0 && draw_y < (HEIGHT as i32) {
                                let c = vic_bank[((char_set_index as i32) * 0x800 + (char_index as i32) * 8 + char_y) as usize];
                                let mask = (c >> (7 - char_x)) & 0x01;
                                if mask != 0 {
                                    buffer[(draw_y * (WIDTH as i32) + pixel_x) as usize] = color;
                                }
                            }
                        }
                    }
                }
            }

            // Draw color layer picture overlay
            /*for (i, index) in color_layer_picture.iter().enumerate() {
                let x = i % CHARS_WIDTH;
                let y = i / CHARS_WIDTH;
                buffer[y * WIDTH + x] = colors[*index as usize]; 
            }*/

            window.update_with_buffer(&buffer);
        }

        last_frame_index = frame_index;
    }
}
