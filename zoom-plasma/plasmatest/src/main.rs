extern crate minifb;
extern crate time;

use minifb::{Key, KeyRepeat, Scale, WindowOptions, Window};

use std::f64::consts::PI;
use std::fs::File;
use std::io::Write;

fn main() {
    const WIDTH: usize = 320;
    const HEIGHT: usize = 200;

    let mut buffer: Box<[u32]> = vec![0; WIDTH * HEIGHT].into_boxed_slice();

    let mut window = Window::new("plasmatest", WIDTH, HEIGHT, WindowOptions {
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

    let palettes = [
        // Red -> yellow
        [
            2,
            8,
            10,
            7
        ],
        // Grayscale
        [
            11,
            12,
            15,
            1
        ],
        // Red -> Purple -> Green (EoD plasma 1)
        [
            2,
            4,
            14,
            5
        ],
        // Blue -> Orange (EoD plasma 2)
        [
            6,
            9,
            8,
            10
        ],
        // Blue -> Green (EoD plasma 3)
        [
            6,
            11,
            14,
            5
        ],
        // Brown -> Green (EoD plasma 4)
        [
            8,
            14,
            3,
            13
        ],
        // Purple -> Green -> Cyan (EoD plasma 5)
        [
            4,
            14,
            5,
            3
        ],
        // Light green -> cyan -> purple -> black
        [
            13,
            14,
            4,
            0
        ]
    ];

    let mut palette = 7;

    let num_chars = 256;
    let num_lines: usize = 8;

    let chars = (0..num_chars).into_iter().map(|i| {
        (0..num_lines).into_iter().map(|y| {
            let yf = (y as f64) / (num_lines as f64);
            let yan = yf * 2.0 * PI;
            (0..4).into_iter().map(|x| {
                let pixel_x = i + x;
                let xf = (pixel_x as f64) / ((num_chars / 4 * 4) as f64);
                let xan = xf * 2.0 * PI;
                let mut f = (yan + xan.sin() * 12.0).cos() * 0.5 + 0.5;
                if f < 0.0 {
                    f = 0.0;
                }
                if f > 1.0 {
                    f = 1.0;
                }
                (f * 3.0).round() as u8
            }).collect::<Vec<_>>().into_boxed_slice()
        }).collect::<Vec<_>>().into_boxed_slice()
    }).collect::<Vec<_>>().into_boxed_slice();

    {
        let mut chars_bytes = Vec::new();
        for line_index in 0..8 {
            for c in chars.iter() {
                let line = &c[line_index];
                let mut b = 0;
                for pixel in line.iter() {
                    b <<= 2;
                    b |= *pixel;
                }
                chars_bytes.push(b);
            }
        }
        let mut file = File::create("../chars.bin").unwrap();
        file.write(&chars_bytes).unwrap();
    }

    // This tab is only used in the generation of other tables
    let scale_tab = (0..256).into_iter().map(|i| {
        let f = (i as f64) / 256.0;
        (f * 2.0 * PI).sin() * 0.5 + 0.7
    }).collect::<Vec<_>>().into_boxed_slice();

    // TODO: Consider using half table and mirroring for these tables
    //  (either for better precision or smaller size)

    // TODO: Fairly certain the offsets/intervals aren't centered around the middle of the screen. Should bake that into the tables somehow

    let x_offset_tab = (0..256).into_iter().map(|i| {
        let scale = scale_tab[i];
        (-0.5 * scale * ((num_chars << 7) as f64)).round() as u16
    }).collect::<Vec<_>>().into_boxed_slice();

    let x_interval_tab = (0..256).into_iter().map(|i| {
        let scale = scale_tab[i];
        64 + (scale * (1024.0 - 64.0)).round() as u16
    }).collect::<Vec<_>>().into_boxed_slice();

    let x_move_tab = (0..256).into_iter().map(|i| {
        let f = (i as f64) / 256.0;
        (((f * 2.0 * PI).sin() * ((num_chars << 8) as f64)) * 0.8).round() as u16
    }).collect::<Vec<_>>().into_boxed_slice();

    /*{
        let tab = x_offset_tab.iter().map(|x| *x as u8).collect::<Vec<_>>();
        let mut file = File::create("x-offset-low-tab.bin").unwrap();
        file.write(&tab).unwrap();
    }

    {
        let tab = x_offset_tab.iter().map(|x| (*x >> 8) as u8).collect::<Vec<_>>();
        let mut file = File::create("x-offset-high-tab.bin").unwrap();
        file.write(&tab).unwrap();
    }

    {
        let tab = x_interval_tab.iter().map(|x| *x as u8).collect::<Vec<_>>();
        let mut file = File::create("x-interval-low-tab.bin").unwrap();
        file.write(&tab).unwrap();
    }

    {
        let tab = x_interval_tab.iter().map(|x| (*x >> 8) as u8).collect::<Vec<_>>();
        let mut file = File::create("x-interval-high-tab.bin").unwrap();
        file.write(&tab).unwrap();
    }

    {
        let tab = x_move_tab.iter().map(|x| *x as u8).collect::<Vec<_>>();
        let mut file = File::create("x-move-low-tab.bin").unwrap();
        file.write(&tab).unwrap();
    }

    {
        let tab = x_move_tab.iter().map(|x| (*x >> 8) as u8).collect::<Vec<_>>();
        let mut file = File::create("x-move-high-tab.bin").unwrap();
        file.write(&tab).unwrap();
    }*/

    let y_offset_tab = (0..256).into_iter().map(|i| {
        let scale = scale_tab[i];
        (-0.5 * scale * (((num_lines * 8) << 8) as f64)).round() as u16
    }).collect::<Vec<_>>().into_boxed_slice();

    let y_interval_tab = (0..256).into_iter().map(|i| {
        let scale = scale_tab[i];
        4 + (scale * (128.0 - 4.0)).round() as u16
    }).collect::<Vec<_>>().into_boxed_slice();

    let y_move_tab = (0..256).into_iter().map(|i| {
        let f = (i as f64) / 256.0;
        (((f * 2.0 * PI).sin() * (((num_lines * 8) << 8) as f64)) * 0.8).round() as u16
    }).collect::<Vec<_>>().into_boxed_slice();

    let y_flow_tab = (0..256).into_iter().map(|i| {
        let f = (i as f64) / 256.0;
        ((((f * 2.0 * PI).sin() * 1.0/* + (f * 2.0 * 2.0 * PI).sin() * 0.5*/) * 0.6 * ((num_lines * 8) as f64)).round() as u8) & ((num_lines as u8) - 1)
    }).collect::<Vec<_>>().into_boxed_slice();

    /*{
        let tab = y_offset_tab.iter().map(|y| *y as u8).collect::<Vec<_>>();
        let mut file = File::create("y-offset-low-tab.bin").unwrap();
        file.write(&tab).unwrap();
    }

    {
        let tab = y_offset_tab.iter().map(|y| (*y >> 8) as u8).collect::<Vec<_>>();
        let mut file = File::create("y-offset-high-tab.bin").unwrap();
        file.write(&tab).unwrap();
    }

    {
        let tab = y_interval_tab.iter().map(|y| *y as u8).collect::<Vec<_>>();
        let mut file = File::create("y-interval-low-tab.bin").unwrap();
        file.write(&tab).unwrap();
    }

    {
        let tab = y_interval_tab.iter().map(|y| (*y >> 8) as u8).collect::<Vec<_>>();
        let mut file = File::create("y-interval-high-tab.bin").unwrap();
        file.write(&tab).unwrap();
    }

    {
        let tab = y_move_tab.iter().map(|y| *y as u8).collect::<Vec<_>>();
        let mut file = File::create("y-move-low-tab.bin").unwrap();
        file.write(&tab).unwrap();
    }

    {
        let tab = y_move_tab.iter().map(|y| (*y >> 8) as u8).collect::<Vec<_>>();
        let mut file = File::create("y-move-high-tab.bin").unwrap();
        file.write(&tab).unwrap();
    }

    {
        let tab = y_flow_tab.iter().map(|y| (*y as u8) << 1).collect::<Vec<_>>();
        let mut file = File::create("y-flow-tab-1.bin").unwrap();
        file.write(&tab).unwrap();
    }*/

    let x_flow_tab = (0..256).into_iter().map(|i| {
        let f = (i as f64) / 256.0;
        (((f * 2.0 * PI).sin() * 1.0/* + (f * 2.0 * 2.0 * PI).sin() * 0.5*/) * 63.0).round() as u8
    }).collect::<Vec<_>>().into_boxed_slice();

    /*{
        let tab = x_flow_tab.iter().map(|x| *x as u8).collect::<Vec<_>>();
        let mut file = File::create("x-flow-tab.bin").unwrap();
        file.write(&tab).unwrap();
    }*/

    let start_time = time::precise_time_s();

    while window.is_open() && !window.is_key_down(Key::Escape) {
        let time = time::precise_time_s() - start_time;

        // Simulate 50fps by quantizing to 20ms intervals
        let frame_index = (time / 0.020) as u32;

        let scale_index = (frame_index * 3 + 74) as u8;
        let x_move_index = (frame_index + 20) as u8;
        let y_move_index = (frame_index - 80) as u8;
        let x_flow_index = (frame_index * 2) as u8 - 90;
        let y_flow_index = (256 - frame_index) as u8;

        // Calculate char indices
        let mut char_index_table = [0; 40];

        let x_offset = x_offset_tab[scale_index as usize] + x_move_tab[x_move_index as usize];
        let x_interval = x_interval_tab[scale_index as usize];
        let mut char_x = x_offset;
        for x in 0..40 {
            /*

            lda char_x_high

            sta screen_pos
            lda char_x_low
            clc
            adc x_interval_low
            sta char_x_low
            lda char_x_high
            adc x_interval_high
            sta char_x_high

            */
            let mut char_index = (char_x >> 7) as u8;
            char_index += x_flow_tab[(((char_x >> 8) as u8) + x_flow_index) as usize];
            char_index_table[x] = char_index;
            if x < 39 {
                char_x += x_interval;
            }
        }

        // Calculate plasma line indices
        let mut line_index_table = [0; HEIGHT];

        let y_interval = y_interval_tab[scale_index as usize];
        let mut line_y = y_offset_tab[scale_index as usize] + y_move_tab[y_move_index as usize];
        for y in 0..HEIGHT {
            /*

            // This is pretty heavy, but I think we can bake the flow parts into the stretcher loop, and only do the zooming here
            lda line_y_high

            // flow part --
            clc
            adc flow_index
            tax
            lda y_flow_tab, x
            clc
            adc line_y_high
            and #$07
            // --
            sta line_index_table_pos

            lda line_y_low
            clc
            adc y_interval_low
            lda line_y_high
            adc y_interval_high
            sta y_line_high

            */

            let scaled_line_y = (line_y >> 8) as u8;
            let mut line_index = scaled_line_y;
            line_index += y_flow_tab[((line_index + y_flow_index) << 1) as usize];
            line_index &= (num_lines as u8) - 1;
            line_index_table[y] = line_index;
            line_y += y_interval;
        }

        // Generate lines (expand single char line)
        let lines = (0..num_lines).into_iter().map(|y| {
            (0..WIDTH / 2).into_iter().map(|x| {
                let char_pos = x >> 2;
                let char_index = char_index_table[char_pos] as usize;
                let char_pixel_index = x & 0x03;
                let c = &chars[char_index];
                let line = &c[y];
                line[char_pixel_index]
            }).collect::<Vec<_>>().into_boxed_slice()
        }).collect::<Vec<_>>().into_boxed_slice();

        // Display plasma
        for y in 0..HEIGHT {
            let line_index = line_index_table[y] as usize;
            let line = &lines[line_index];
            for x in 0..WIDTH / 2 {
                let c = line[x] as usize;
                let argb = 0xff000000 | colors[palettes[palette][c]];
                buffer[y * WIDTH + x * 2] = argb;
                buffer[y * WIDTH + x * 2 + 1] = argb;
            }
        }

        // Display lines
        /*for y in 0..num_lines {
            let line = &lines[y];
            for x in 0..WIDTH / 2 {
                let c = line[x] as usize;
                let argb = 0xff000000 | colors[palettes[palette][c]];
                buffer[y * WIDTH + x * 2] = argb;
                buffer[y * WIDTH + x * 2 + 1] = argb;
            }
        }*/

        // Display lines repeated
        /*for y in 0..HEIGHT {
            let line = &lines[y & (num_lines - 1)];
            for x in 0..WIDTH / 2 {
                let c = line[x] as usize;
                let argb = 0xff000000 | colors[palettes[palette][c]];
                buffer[y * WIDTH + x * 2] = argb;
                buffer[y * WIDTH + x * 2 + 1] = argb;
            }
        }*/

        if window.is_key_pressed(Key::Up, KeyRepeat::Yes) {
            palette = (palette + 1) % palettes.len();
        } else if window.is_key_pressed(Key::Down, KeyRepeat::Yes) {
            if palette > 0 {
                palette -= 1;
            } else {
                palette = palettes.len() - 1;
            }
        }

        window.update_with_buffer(&buffer);
    }
}

fn sharp(x: f64) -> f64 {
    let x = x / (2.0 * PI);
    ((x - x.floor()) * 2.0 - 1.0).abs() * 2.0 - 1.0
}
