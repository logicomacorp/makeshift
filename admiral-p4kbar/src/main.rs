use std::env::args;
use std::fs::File;
use std::io::{Read, stdout, Write};
use std::path::Path;
use std::time::Instant;

const NORMALIZATION_INTERVAL_BITS: u32 = 15;
const NORMALIZATION_INTERVAL_LOWER_BOUND: u32 = 1 << NORMALIZATION_INTERVAL_BITS;
const PREDICTION_BITS: u32 = 8;
const INITIAL_PREDICTIONS: u8 = 110;
const LEARNING_RATE: u32 = 4;

const MATCH_LEN_BITS: u32 = 8;
const MATCH_OFFSET_BITS: u32 = 12;
const LITERAL_BITS: u32 = 8;

const NUM_MATCH_LEN_CONTEXTS: usize = 1 << MATCH_LEN_BITS;
const MATCH_LEN_CONTEXTS: usize = 0;
const NUM_MATCH_OFFSET_CONTEXTS: usize = 1 << MATCH_OFFSET_BITS;
const MATCH_OFFSET_CONTEXTS: usize = MATCH_LEN_CONTEXTS + NUM_MATCH_LEN_CONTEXTS;
const NUM_LITERAL_CONTEXTS: usize = 1 << LITERAL_BITS;
const LITERAL_CONTEXTS: usize = MATCH_OFFSET_CONTEXTS + NUM_MATCH_OFFSET_CONTEXTS;
const NUM_TOTAL_MODEL_CONTEXTS: usize = NUM_MATCH_LEN_CONTEXTS + NUM_MATCH_OFFSET_CONTEXTS + NUM_LITERAL_CONTEXTS;

const MAX_MAX_ARRIVALS_PER_POSITION: usize = 16;

#[derive(Clone, Copy)]
enum Profile {
    Optimal,
    Balanced,
    Instant,
}

fn main() {
    // Garish banner in order to more easily see the packer output amongst all the crap KickAssembler spits out :)
    println!("//------------------------------------------------------");
    println!("//------------------------------------------------------");
    println!("//        Admiral P4kbar v2 by ferris / logicoma        ");
    println!("//------------------------------------------------------");
    println!("//------------------------------------------------------");
    println!();

    let total_start_time = Instant::now();

    let input_file_name = args().skip(1).nth(0).expect("Input file name argument missing");
    let output_file_name = args().skip(1).nth(1).expect("Output file name argument missing");
    let final_coder_state_output_file_name = args().skip(1).nth(2).expect("Final coder state output file name argument missing");
    let report_file_name = args().skip(1).nth(3).expect("Report file name argument missing");
    let profile_name = args().skip(1).nth(4).expect("Profile argument missing");

    // Read input file
    let input = {
        let mut input = Vec::new();
        File::open(input_file_name).expect("Couldn't open input file").read_to_end(&mut input).expect("Couldn't read input file");
        input
    };

    // Set params from profile
    let profile = match profile_name.as_str() {
        "optimal" => Profile::Optimal,
        "balanced" => Profile::Balanced,
        "instant" => Profile::Instant,
        _ => panic!("Unrecognized profile: {}", profile_name)
    };

    let (max_arrivals_per_position, min_match_len, min_greedy_len) = match profile {
        Profile::Optimal => (MAX_MAX_ARRIVALS_PER_POSITION, 1, None),
        Profile::Balanced => (2, 2, Some(1 << (MATCH_LEN_BITS - 2))),
        Profile::Instant => (1, 3, Some(4)),
    };

    // Compress
    let uncompressed_size = input.len();
    let compress_start_time = Instant::now();

    let (compressed, encoded_packets, final_coder_state) = compress(&input, max_arrivals_per_position, min_match_len, min_greedy_len, |pos, best_size| {
        let percent_complete = pos * 100 / uncompressed_size;
        let ratio = (1.0 - (best_size as f64) / (uncompressed_size as f64)) * 100.0;
        print!("\rCompressing ... {:3}% | {} -> {} bytes ({:.*}%)", percent_complete, uncompressed_size, best_size, 2, ratio);
        stdout().flush().expect("Couldn't flush stdout");
    });

    let compress_duration = compress_start_time.elapsed();
    let compress_elapsed_time = (compress_duration.as_secs() as f64) + (compress_duration.subsec_nanos() as f64) / 1.0e9;
    let compress_data_rate_kb_s = (uncompressed_size as f64) / 1024.0 / (compress_elapsed_time as f64);

    println!(" in {:.*}s (~{:.*}kb/s)", 2, compress_elapsed_time, 2, compress_data_rate_kb_s);
    println!("{} bytes difference, {} packets, final coder state: 0x{:04x}", (uncompressed_size as i32) - (compressed.len() as i32), encoded_packets.len(), final_coder_state & 0xffff);
    println!();

    // Decompression check
    let decompressed = decompress(&compressed, encoded_packets.len(), final_coder_state);
    if decompressed == input {
        println!("Decompression check OK");
        println!();
    } else {
        panic!("Decompression check FAILED");
    }

    // Write output
    File::create(&output_file_name).expect("Couldn't open output file").write(&compressed).expect("Couldn't write output file");
    println!("Output written to {}", output_file_name);

    // Write output
    File::create(&final_coder_state_output_file_name).expect("Couldn't open output file").write(&[final_coder_state as u8, (final_coder_state >> 8) as u8]).expect("Couldn't write output file");
    println!("Final coder state output written to {}", final_coder_state_output_file_name);

    // Write report
    generate_report(&report_file_name, &input, &encoded_packets);
    println!("Report written to {}", report_file_name);
    println!();

    // Final stats
    let total_duration = total_start_time.elapsed();
    let total_elapsed_time = (total_duration.as_secs() as f64) + (total_duration.subsec_nanos() as f64) / 1.0e9;

    let total_compressed_size = compressed.len();

    let total_ratio = (1.0 - (total_compressed_size as f64) / (uncompressed_size as f64)) * 100.0;

    println!("Final results:");
    println!("  {} -> {} bytes ({:.*}%)", uncompressed_size, total_compressed_size, 2, total_ratio);
    println!("  completed in {:.*}s", 2, total_elapsed_time);

    println!();
}

#[derive(Clone)]
struct Model {
    contexts: Vec<u8>,
    last_match_offset: u32,
}

impl Model {
    fn new() -> Model {
        Model {
            contexts: vec![INITIAL_PREDICTIONS; NUM_TOTAL_MODEL_CONTEXTS],
            last_match_offset: 0,
        }
    }

    fn estimate_packet_cost_bits(&self, packet: &Packet) -> f64 {
        let mut ret = 0.0;
        ret += self.estimate_value_cost_bits(packet.match_len, MATCH_LEN_BITS, MATCH_LEN_CONTEXTS);
        if packet.match_len > 0 {
            ret += self.estimate_value_cost_bits(if packet.match_offset == self.last_match_offset { 0 } else { packet.match_offset }, MATCH_OFFSET_BITS, MATCH_OFFSET_CONTEXTS);
        } else {
            ret += self.estimate_value_cost_bits(packet.literal, LITERAL_BITS, LITERAL_CONTEXTS);
        }
        ret
    }

    fn estimate_value_cost_bits(&self, value: u32, bits: u32, contexts: usize) -> f64 {
        let mut ret = 0.0;
        let mut context_bits = 1;
        for i in 0..bits {
            let symbol = (value >> (bits - 1 - i)) & 0x01;
            let context = contexts + (context_bits as usize);
            ret += self.estimate_symbol_cost_bits(symbol, context);
            context_bits = (context_bits << 1) | symbol;
        }
        ret
    }

    fn estimate_symbol_cost_bits(&self, symbol: u32, context: usize) -> f64 {
        let prediction = if symbol == 1 {
            self.contexts[context] as u32
        } else {
            (1 << PREDICTION_BITS) - (self.contexts[context] as u32)
        };
        -((prediction as f64) / ((1 << PREDICTION_BITS) as f64)).log2()
    }

    fn update_with_packet(&mut self, packet: &Packet) {
        self.update_value(packet.match_len, MATCH_LEN_BITS, MATCH_LEN_CONTEXTS);
        if packet.match_len > 0 {
            self.update_value(if packet.match_offset == self.last_match_offset { 0 } else { packet.match_offset }, MATCH_OFFSET_BITS, MATCH_OFFSET_CONTEXTS);
            self.last_match_offset = packet.match_offset;
        } else {
            self.update_value(packet.literal, LITERAL_BITS, LITERAL_CONTEXTS);
        }
    }

    fn update_value(&mut self, value: u32, bits: u32, contexts: usize) {
        let mut context_bits = 1;
        for i in 0..bits {
            let symbol = (value >> (bits - 1 - i)) & 0x01;
            let context = contexts + (context_bits as usize);
            self.update(symbol, context);
            context_bits = (context_bits << 1) | symbol;
        }
    }

    fn update(&mut self, symbol: u32, context: usize) {
        if symbol == 1 {
            let mut adjustment = (((1 << PREDICTION_BITS) - (self.contexts[context] as u32)) >> LEARNING_RATE) as u8;
            if adjustment == 0 {
                adjustment = 1;
            }
            self.contexts[context] += adjustment;
            if self.contexts[context] == 0 {
                self.contexts[context] -= 1;
            }
        } else {
            let mut adjustment = self.contexts[context] >> LEARNING_RATE;
            if adjustment == 0 {
                adjustment = 1;
            }
            self.contexts[context] -= adjustment;
            if self.contexts[context] == 0 {
                self.contexts[context] += 1;
            }
        }
    }
}

#[derive(Clone)]
struct Packet {
    match_len: u32,
    match_offset: u32,
    literal: u32,
}

#[derive(Clone)]
struct ArrivalSource {
    position: usize,
    arrival_index: usize,
}

#[derive(Clone)]
struct Arrival {
    packet: Packet,
    source: Option<ArrivalSource>,
    cost_bits: f64,
    model: Model,
}

struct EncodeState {
    model: Model,
    symbols: Vec<(u8, u8)>,
    output_bits: Vec<u8>,
    bit_buffer: u32,
    bit_buffer_pos: u32,
    output: Vec<u8>,
}

struct EncodedPacket {
    packet: Packet,
    cost_bits: f64,
}

fn compress<F>(input: &[u8], max_arrivals_per_position: usize, min_match_len: usize, min_greedy_len: Option<u32>, f: F) -> (Vec<u8>, Vec<EncodedPacket>, u32) where F: Fn(usize, usize) {
    // Parse
    let mut position_arrivals: Vec<[Option<Arrival>; MAX_MAX_ARRIVALS_PER_POSITION]> = vec![[None, None, None, None, None, None, None, None, None, None, None, None, None, None, None, None]; input.len() + 1];

    let mut input_pos = 0;
    while input_pos < input.len() {
        if (input_pos & 0x3f) == 0 {
            f(input_pos, (position_arrivals[input_pos][0].as_ref().map(|x| x.cost_bits).unwrap_or(0.0) / 8.0) as _);
        }

        // Test literal-only packet
        let literal = input[input_pos];
        parse_test_packet(Packet {
            match_len: 0,
            match_offset: 0,
            literal: literal as _,
        }, input_pos, input_pos + 1, &mut position_arrivals, max_arrivals_per_position);

        // Match search
        let min_match_len = min_match_len;
        let mut max_match_len = (1 << MATCH_LEN_BITS) - 1;
        if max_match_len > input.len() - 1 - input_pos {
            max_match_len = input.len() - 1 - input_pos;
        }
        let min_match_offset = 1;
        let mut max_match_offset = (1 << MATCH_OFFSET_BITS) - 1;
        if max_match_offset > input_pos {
            max_match_offset = input_pos;
        }

        let mut match_packets = Vec::new();

        if input_pos > min_match_offset {
            for match_search_offset in min_match_offset..=max_match_offset {
                let mut match_search_len = 0;
                while match_search_len < max_match_len && input[input_pos - match_search_offset + match_search_len] == input[input_pos + match_search_len] {
                    match_search_len += 1;
                }

                if match_search_len >= min_match_len {
                    // Build match packet
                    match_packets.push(Packet {
                        match_len: match_search_len as _,
                        match_offset: match_search_offset as _,
                        literal: 0,
                    });
                }
            }
        }

        if let Some(min_greedy_len) = min_greedy_len {
            if let Some(packet) = match_packets.iter().filter(|x| x.match_len >= min_greedy_len).max_by_key(|x| x.match_len) {
                let match_len = packet.match_len as usize;
                parse_test_packet(packet.clone(), input_pos, input_pos + match_len, &mut position_arrivals, max_arrivals_per_position);
                input_pos += match_len;
                continue;
            }
        }

        for packet in match_packets.into_iter() {
            let match_len = packet.match_len as usize;
            parse_test_packet(packet, input_pos, input_pos + match_len, &mut position_arrivals, max_arrivals_per_position);
        }

        input_pos += 1;
    }

    // Build packet list from parsed arrivals
    let mut packets = Vec::new();
    let mut position = input.len();
    let mut arrival_index = 0;
    loop {
        let best_arrival = position_arrivals[position][arrival_index].as_ref().expect(&format!("No arrival for position {} :(", position));
        packets.push(best_arrival.packet.clone());

        if let Some(ref source) = best_arrival.source {
            position = source.position;
            arrival_index = source.arrival_index;
        } else {
            break;
        };
    }

    // Encode packets
    let mut state = EncodeState {
        model: Model::new(),
        symbols: Vec::new(),
        output_bits: Vec::new(),
        bit_buffer: 0,
        bit_buffer_pos: 0,
        output: Vec::new(),
    };

    let mut encoded_packets = Vec::new();

    for packet in packets.iter().rev() {
        encode_write_bits(&mut state, packet.match_len, MATCH_LEN_BITS, MATCH_LEN_CONTEXTS);
        if packet.match_len > 0 {
            let offset = if packet.match_offset == state.model.last_match_offset { 0 } else { packet.match_offset };
            encode_write_bits(&mut state, offset, MATCH_OFFSET_BITS, MATCH_OFFSET_CONTEXTS);
        } else {
            encode_write_bits(&mut state, packet.literal, LITERAL_BITS, LITERAL_CONTEXTS);
        }

        encoded_packets.push(EncodedPacket {
            packet: packet.clone(),
            cost_bits: state.model.estimate_packet_cost_bits(packet),
        });

        state.model.update_with_packet(packet);
    }

    // Flush state (this performs actual encoding)
    let final_coder_state = encode_flush(&mut state);

    f(input.len(), state.output.len());

    (state.output, encoded_packets, final_coder_state)
}

fn parse_test_packet(packet: Packet, input_pos: usize, arrival_pos: usize, position_arrivals: &mut Vec<[Option<Arrival>; MAX_MAX_ARRIVALS_PER_POSITION]>, max_arrivals_per_position: usize) {
    if input_pos == 0 {
        parse_test_packet_from_arrival(packet, input_pos, arrival_pos, None, 0, position_arrivals, max_arrivals_per_position);
    } else {
        // Test packet as if it came from all of the arrivals at the current pos
        for arrival_index in 0..max_arrivals_per_position {
            // TODO: Do we actually need to clone here?
            if let Some(from_arrival) = position_arrivals[input_pos][arrival_index].clone() {
                parse_test_packet_from_arrival(packet.clone(), input_pos, arrival_pos, Some(from_arrival), arrival_index, position_arrivals, max_arrivals_per_position);
            }
        }
    }
}

fn parse_test_packet_from_arrival(packet: Packet, input_pos: usize, arrival_pos: usize, from_arrival: Option<Arrival>, from_arrival_index: usize, position_arrivals: &mut Vec<[Option<Arrival>; MAX_MAX_ARRIVALS_PER_POSITION]>, max_arrivals_per_position: usize) {
    // Build potential arrival
    let (source, from_arrival_cost_bits, mut model) = if let Some(from_arrival) = from_arrival {
        (Some(ArrivalSource {
            position: input_pos,
            arrival_index: from_arrival_index,
        }), from_arrival.cost_bits, from_arrival.model.clone())
    } else {
        (None, 0.0, Model::new())
    };
    let cost_bits = from_arrival_cost_bits + model.estimate_packet_cost_bits(&packet);
    model.update_with_packet(&packet);
    let arrival = Arrival {
        packet: packet,
        source: source,
        cost_bits: cost_bits,
        model: model,
    };
    // Store arrival if valuable
    let mut insert_index = 0;
    while insert_index < max_arrivals_per_position {
        if let Some(ref best_arrival) = position_arrivals[arrival_pos][insert_index] {
            if arrival.cost_bits < best_arrival.cost_bits {
                break;
            }
        } else {
            break;
        }
        insert_index += 1;
    }
    if insert_index < max_arrivals_per_position {
        // Move worse arrivals, if necessary
        let mut move_index = max_arrivals_per_position - 1;
        while move_index > insert_index {
            position_arrivals[arrival_pos][move_index] = position_arrivals[arrival_pos][move_index - 1].clone();
            move_index -= 1;
        }
        // Insert arrival
        position_arrivals[arrival_pos][insert_index] = Some(arrival);
    }
}

fn encode_write_bits(state: &mut EncodeState, value: u32, bits: u32, contexts: usize) {
    let mut context_bits = 1;
    for i in 0..bits {
        let symbol = (value >> (bits - 1 - i)) & 0x01;
        let context = contexts + (context_bits as usize);
        encode_write_bit(state, symbol, state.model.contexts[context] as _);
        context_bits = (context_bits << 1) | symbol;
    }
}

fn encode_write_bit(state: &mut EncodeState, bit: u32, prediction: u32) {
    state.symbols.push((bit as _, prediction as _));
}

fn encode_flush(state: &mut EncodeState) -> u32 {
    let mut x = NORMALIZATION_INTERVAL_LOWER_BOUND;

    for (bit, prediction) in state.symbols.iter().cloned().rev() {
        let bit = bit as u32;
        let prediction = prediction as u32;

        let (prediction, bias) = if bit == 1 {
            (prediction, 0)
        } else {
            ((1 << PREDICTION_BITS) - prediction, prediction)
        };

        // Renormalize
        let x_max = ((NORMALIZATION_INTERVAL_LOWER_BOUND >> PREDICTION_BITS) << 1) * prediction;
        while x >= x_max {
            state.output_bits.push((x & 0x01) as _);
            x >>= 1;
        }

        // Encode symbol
        x = ((x / prediction) << PREDICTION_BITS) + (x % prediction) + bias;
    }

    // Flush output bits
    for bit in state.output_bits.iter().cloned().rev() {
        if state.bit_buffer_pos == 8 {
            state.output.push(state.bit_buffer as _);
            state.bit_buffer = 0;
            state.bit_buffer_pos = 0;
        }

        state.bit_buffer |= (bit as u32) << state.bit_buffer_pos;
        state.bit_buffer_pos += 1;
    }

    if state.bit_buffer_pos > 0 {
        state.output.push(state.bit_buffer as _);
    }

    x
}

struct DecodeState {
    x_low: u8,
    x_high: u8,
    model: Model,
    bit_buffer: u8,
    bit_buffer_pos: u8,
    input_pos: usize,
}

fn decompress(input: &[u8], num_packets: usize, final_coder_state: u32) -> Vec<u8> {
    let mut state = DecodeState {
        x_low: final_coder_state as _,
        x_high: (final_coder_state >> 8) as _,
        model: Model::new(),
        bit_buffer: 0,
        bit_buffer_pos: 0,
        input_pos: 0,
    };

    let mut output = Vec::new();

    for _ in 0..num_packets {
        let length = decode_read_bits(&mut state, 1 << MATCH_LEN_BITS, MATCH_LEN_CONTEXTS, input);
        if length > 0 {
            let mut offset = decode_read_bits(&mut state, 1 << MATCH_OFFSET_BITS, MATCH_OFFSET_CONTEXTS, input);
            if offset == 0 {
                offset = state.model.last_match_offset;
            }
            state.model.last_match_offset = offset;

            let mut match_pos = output.len() - (offset as usize);
            for _ in 0..length {
                output.push(output[match_pos]);
                match_pos += 1;
            }
        } else {
            let literal = decode_read_bits(&mut state, 1 << LITERAL_BITS, LITERAL_CONTEXTS, input);
            output.push(literal as _);
        }
    }

    output
}

fn decode_read_bits(state: &mut DecodeState, max_context_bits: u32, contexts: usize, input: &[u8]) -> u32 {
    let mut context_bits = 1;
    while context_bits < max_context_bits {
        let context = contexts + (context_bits as usize);
        let prediction = state.model.contexts[context];

        let symbol = decode_bit(state, prediction as _, input);

        state.model.update(symbol, context);

        context_bits = (context_bits << 1) | symbol;
    }
    context_bits - max_context_bits
}

fn decode_bit(state: &mut DecodeState, prediction: u8, input: &[u8]) -> u32 {
    // Initial symbol (lda, sta)
    let mut symbol = 0;
    // Initial bias (sta)
    let mut bias = 0;

    // Decode symbol + prediction/bias for state adjustment
    let prediction = if prediction > state.x_low {
        // Increment symbol (inc)
        symbol += 1;
        prediction
    } else {
        // Set bias to prediction (lda, sta)
        bias = prediction;
        ((1 << PREDICTION_BITS) - (prediction as u16)) as u8
    };

    // x = prediction * x_high (8x8 bit multiply = 16 bit product)
    let (mut product_low, mut product_high) = {
        let product = (state.x_high as u16) * (prediction as u16);
        (product as u8, (product >> 8) as u8)
    };
    // Add back original state low bits (clc, lda, adc, sta, lda, adc, sta)
    let mut carry = 0;
    let sum = (product_low as u16) + (state.x_low as u16) + carry;
    carry = sum >> 8;
    product_low = sum as _;
    let sum = (product_high as u16) + 0 + carry;
    //carry = sum >> 8;
    product_high = sum as _;
    // Subtract bias (sec, lda, sbc, sta, lda)
    carry = 1;
    let sum = (product_low as u16) + ((bias ^ 0xff) as u16) + carry;
    carry = (sum >> 8) & 0x01;
    state.x_low = sum as _;
    let sum = (product_high as u16) + (0 ^ 0xff) + carry;
    //carry = (sum >> 8) & 0x01;
    state.x_high = sum as _;
    let mut sign = state.x_high >> 7;

    // Renormalize
    while sign == 0 {
        // input bit + asl, ora
        let new_carry = (state.x_low >> 7) as u16;
        state.x_low = (state.x_low << 1) | decode_input_bit(state, input);
        carry = new_carry;
        // rol
        //let new_carry = (state.x_high >> 7) as u16;
        state.x_high = (state.x_high << 1) | (carry as u8);
        //carry = new_carry;
        sign = state.x_high >> 7;
    }

    symbol
}

fn decode_input_bit(state: &mut DecodeState, input: &[u8]) -> u8 {
    if state.bit_buffer_pos == 0 {
        state.bit_buffer = input[state.input_pos] as _;
        state.input_pos += 1;
        state.bit_buffer_pos = 8;
    }

    let ret = state.bit_buffer & 0x01;
    state.bit_buffer >>= 1;
    state.bit_buffer_pos -= 1;

    ret
}

fn generate_report<P: AsRef<Path>>(output_file_name: P, input: &[u8], encoded_packets: &[EncodedPacket]) {
    let mut output = File::create(output_file_name).expect("Couldn't open output file");
    write!(output, "<html>
<head>
    <title>admiral p4kbar report</title>
    <style>
body {{
    background-color: #222;
    color: #fff;
    font-family: monospace;
}}
table, th, td {{
    background-color: #333;
    border: 1px solid #222;
    border-collapse: collapse;
}}
th, td {{
    padding-left: 4px;
    padding-right: 4px;
    font-size: 11px;
}}
.heatmap-0 {{
    background-color: #000560;
    color: #fff;
    font-size: 11px;
    text-shadow: 0 1px 2px #000;
}}
.heatmap-1 {{
    background-color: #023d9a;
    color: #fff;
    font-size: 11px;
    text-shadow: 0 1px 2px #000;
}}
.heatmap-2 {{
    background-color: #005fd3;
    color: #fff;
    font-size: 11px;
    text-shadow: 0 1px 2px #000;
}}
.heatmap-3 {{
    background-color: #0186c0;
    color: #fff;
    font-size: 11px;
    text-shadow: 0 1px 2px #000;
}}
.heatmap-4 {{
    background-color: #4ab03d;
    color: #fff;
    font-size: 11px;
    text-shadow: 0 1px 2px #000;
}}
.heatmap-5 {{
    background-color: #b5d000;
    color: #fff;
    font-size: 11px;
    text-shadow: 0 1px 2px #000;
}}
.heatmap-6 {{
    background-color: #ebd109;
    color: #fff;
    font-size: 11px;
    text-shadow: 0 1px 2px #000;
}}
.heatmap-7 {{
    background-color: #fba70f;
    color: #fff;
    font-size: 11px;
    text-shadow: 0 1px 2px #000;
}}
.heatmap-8 {{
    background-color: #ee0000;
    color: #fff;
    font-size: 11px;
    text-shadow: 0 1px 2px #000;
}}
.heatmap-9 {{
    background-color: #950000;
    color: #fff;
    font-size: 11px;
    text-shadow: 0 1px 2px #000;
}}
    </style>
</head>
<body>
    <h2>admiral p4kbar report</h2>
    <p>please, don't make me say the tagline..</p>").unwrap();

    write!(output, "<div>
        <h3>key</h3>
        <p>").unwrap();
    for i in 0..10 {
        if i < 9 {
            write!(output, "<span class=\"heatmap-{}\">00</span> {}-{} bits/byte<br />", i, i, i + 1).unwrap();
        } else {
            write!(output, "<span class=\"heatmap-{}\">00</span> >={} bits/byte<br />", i, i).unwrap();
        }
    }

    write!(output, "</p>
    </div>").unwrap();

    write!(output, "<div>
        <h3>heatmap</h3>
        <p>").unwrap();

    let mut byte_index = 0;
    for encoded_packet in encoded_packets {
        let packet = &encoded_packet.packet;
        let num_bytes = if packet.match_len == 0 { 1 } else { packet.match_len };
        let uncompressed_bits = num_bytes * 8;
        let compressed_bits = encoded_packet.cost_bits;
        let bits_per_byte = compressed_bits / (num_bytes as f64);
        let mut byte_size = (8.0 * compressed_bits / (uncompressed_bits as f64)).floor() as u32;
        if byte_size > 9 {
            byte_size = 9;
        }

        let title = if packet.match_len == 0 {
            format!("literal (byte: 0x{:02x})", packet.literal)
        } else {
            format!("match (offset: {}, length: {})", packet.match_offset, packet.match_len)
        };
        let start_span = format!("<span title=\"{}\ntotal comp. bits: {}\nbits/byte: {}\" class=\"heatmap-{}\">", title, compressed_bits, bits_per_byte, byte_size);
        write!(output, "{}", start_span).unwrap();

        for _ in 0..num_bytes {
            let row_len = 64;
            let x = byte_index % row_len;

            let byte = input[byte_index];

            write!(output, "{:02x}", byte).unwrap();

            if x == row_len - 1 {
                write!(output, "</span>").unwrap();
                write!(output, "&nbsp;<br />").unwrap();
                write!(output, "{}", start_span).unwrap();
            }

            byte_index += 1;
        }

        write!(output, "</span>").unwrap();
    }

    write!(output, "
        </p>
    </div>").unwrap();

    write!(output, "</body>
</html>").unwrap();
}
