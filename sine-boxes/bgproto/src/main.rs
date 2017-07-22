extern crate minifb;
extern crate time;
extern crate image;

use minifb::{Key, Scale, WindowOptions, Window};

use image::{GenericImage, Pixel, Rgb};

use std::f64::consts::PI;
use std::fs::File;
use std::io::Write;

fn main() {
    const WIDTH: usize = 320;
    const HEIGHT: usize = 200;

    let mut buffer = vec![0; WIDTH * HEIGHT].into_boxed_slice();

    let mut window = Window::new(
        "BG stuff",
        WIDTH,
        HEIGHT,
        WindowOptions {
            borderless: false,
            title: true,
            resize: false,
            scale: Scale::X2,
        }).unwrap();

    const CHAR_SIZE: usize = 8;

    const TILE_SIZE: usize = 16;

    let tile_image = image::open("brogicoma-thick.png").unwrap();
    let mut palette = Vec::new();
    // Force palette for easier ramps in the intro
    palette.push(Rgb {
        data: [255, 255, 255]
    });
    palette.push(Rgb {
        data: [0, 0, 0]
    });
    /*palette.push(Rgb {
        data: [64, 49, 141]
    });
    palette.push(Rgb {
        data: [139, 63, 150]
    });
    palette.push(Rgb {
        data: [103, 182, 189]
    });
    palette.push(Rgb {
        data: [255, 255, 255]
    });*/
    let mut tiles = Vec::new();
    for y in 0..TILE_SIZE {
        let mut acc = 0;

        for x in 0..TILE_SIZE {
            let p = tile_image.get_pixel(x as _, y as _).to_rgb();
            /*if !palette.contains(&p) {
                palette.push(p);
            }*/
            let c = palette.iter().position(|x| x.data == p.data).unwrap() as u16;

            acc >>= 1;
            acc |= c << 15;
        }

        tiles.push((acc >> 8) as u8);
        tiles.push(acc as u8);
    }
    {
        let mut file = File::create("../tiles.bin").unwrap();
        file.write(&tiles).unwrap();
    }

    let mut charset = Vec::new();
    for pos_y in 0..16 {
        for pos_x in 0..16 {
            let shift_x = pos_x & 0x0f;
            let shift_y = pos_y & 0x0f;

            for char_y in 0..CHAR_SIZE {
                let mut acc = 0;

                for char_x in 0..CHAR_SIZE {
                    let offset_x = (char_x + shift_x) % TILE_SIZE;
                    let offset_y = (char_y + shift_y) % TILE_SIZE;

                    let tiles_index = ((offset_y as usize)) * 2;
                    let tiles_row = ((tiles[tiles_index] as u16) << 8) | (tiles[tiles_index + 1] as u16);
                    let tiles_row_shift = offset_x;

                    let c = ((tiles_row >> tiles_row_shift) as u8) & 0x01;

                    acc <<= 1;
                    acc |= c;
                }

                charset.push(acc);
            }
        }
    }
    {
        let mut file = File::create("../charset.bin").unwrap();
        file.write(&charset).unwrap();
    }

    let mut dynamic_tab = Vec::new();
    for i in 0..256 {
        let x =
            ((((i as f64) / 256.0 * 2.0 * 2.0 * PI).sin() * 31.0) as i32) +
            ((((((i + 40) as f64) / 256.0 * 3.0 * 2.0 * PI).sin() * 31.0) as i32) << 4);
        dynamic_tab.push(x as u8);
    }
    {
        let mut file = File::create("../dynamic-tab.bin").unwrap();
        file.write(&dynamic_tab).unwrap();
    }

    let start_time = time::precise_time_s();

    while window.is_open() && !window.is_key_down(Key::Escape) {
        let time = time::precise_time_s() - start_time;

        // Simulate 50fps by quantizing to 20ms intervals
        let frame_index = (time / 0.020) as i32;

        for tile_y in 0..(HEIGHT / CHAR_SIZE) as i32 {
            for tile_x in 0..(WIDTH / CHAR_SIZE) as i32 {

                let constant =
                    (tile_x * 8 +
                    ((tile_y * 8) << 4)) as u8;

                let dynamic_index = (frame_index + ((tile_x / 2 + tile_y) & 0xff)) & 0xff;
                let dynamic = dynamic_tab[dynamic_index as usize];

                let char_index = constant + dynamic;

                let char_offset = (char_index as usize) * 8;

                for y in 0..CHAR_SIZE {
                    for x in 0..CHAR_SIZE {
                        let c = (charset[char_offset + y] >> (7 - x)) & 0x01;

                        let color = palette[c as usize];
                        buffer[((tile_y * (CHAR_SIZE as i32) + (y as i32)) * (WIDTH as i32) + tile_x * (CHAR_SIZE as i32) + (x as i32)) as usize] =
                            0xff000000 | ((color.data[0] as u32) << 16) | ((color.data[1] as u32) << 8) | (color.data[2] as u32);
                    }
                }
            }
        }

        window.update_with_buffer(&buffer);
    }

    println!("Hello, world!");
}
