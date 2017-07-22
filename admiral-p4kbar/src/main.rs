extern crate typed_arena;

extern crate fnv;

use typed_arena::Arena;

use fnv::FnvHashMap;

use std::cmp::Ordering;
use std::collections::{BTreeMap, HashMap, BTreeSet};
use std::env;
use std::fmt;
use std::fs::File;
use std::io::{Read, Write, stdout};
use std::path::Path;
use std::time::Instant;

#[derive(Clone, Copy)]
enum SpeedSetting {
    Optimal,
    Instant,
}

fn main() {
    // Garish banner in order to more easily see the packer output amongst all the crap KickAssembler spits out :)
    println!("//------------------------------------------------------");
    println!("//------------------------------------------------------");
    println!("//       Admiral P4kbar v1.337 by Ferris / Wheel        ");
    println!("//------------------------------------------------------");
    println!("//------------------------------------------------------");
    println!();

    let input_file_name = env::args().skip(1).nth(0).expect("Input file argument missing");
    let output_file_name = env::args().skip(1).nth(1).expect("Output file argument missing");
    let report_file_name = env::args().skip(1).nth(2).expect("Report file argument missing");
    let speed_setting_name = env::args().skip(1).nth(3).expect("Speed setting argument missing");

    let speed_setting = match speed_setting_name.as_str() {
        "optimal" => SpeedSetting::Optimal,
        "instant" => SpeedSetting::Instant,
        _ => panic!("Unrecognized speed setting: {}", speed_setting_name)
    };

    let total_start_time = Instant::now();

    // Read input file
    let input = {
        let mut input = Vec::new();
        File::open(input_file_name).expect("Couldn't open input file").read_to_end(&mut input).expect("Couldn't read input file");
        input
    };

    let uncompressed_size = input.len();

    // Parse
    let parse_start_time = Instant::now();

    let edge_lists = parse(input.clone(), speed_setting, |pos| {
        let percent_complete = pos * 100 / uncompressed_size;
        print!("\rParsing ... {}%", percent_complete);
        stdout().flush().expect("Couldn't flush stdout");
    });
    println!();

    let parse_duration = parse_start_time.elapsed();
    let parse_elapsed_time = (parse_duration.as_secs() as f64) + (parse_duration.subsec_nanos() as f64) / 1.0e9;
    let parse_data_rate_kb_s = (uncompressed_size as f64) / 1024.0 / (parse_elapsed_time as f64);

    let num_edges = edge_lists.iter().fold(0, |acc, edge_list| acc + edge_list.len());

    println!("  {} edges built in {:.*}s (~{:.*}kb/s)", num_edges, 2, parse_elapsed_time, 2, parse_data_rate_kb_s);
    println!();

    // Use gamma encoding as a heuristic to find the seed edge path
    let seed_encoding = TableEncoding::default();
    let (seed_path, seed_path_size) = compress_and_print_stats(&edge_lists, &seed_encoding, uncompressed_size, "Finding seed edge path");
    println!();

    // Refine encoding and recompress until no more improvements are made
    let mut encoding = None;
    let mut path: Option<(Vec<Edge>, u32)> = None;

    match speed_setting {
        SpeedSetting::Optimal => {
            for _ in 0..65536 {
                print!("Searching for optimal encoding ... ");
                stdout().flush().expect("Couldn't flush stdout");

                let encoding_search_start_time = Instant::now();

                let new_encoding = TableEncoding::new(path.as_ref().map(|path| &path.0).unwrap_or(&seed_path));

                let encoding_search_duration = encoding_search_start_time.elapsed();
                let encoding_search_elapsed_time = (encoding_search_duration.as_secs() as f64) + (encoding_search_duration.subsec_nanos() as f64) / 1.0e9;

                println!("found in {:.*}s", 2, encoding_search_elapsed_time);
                println!("  {}", new_encoding);

                let (new_path, new_path_size) = compress_and_print_stats(&edge_lists, &new_encoding, uncompressed_size, "Compressing");
                if path.as_ref().map(|&(_, best_path_size)| new_path_size < best_path_size).unwrap_or(true) {
                    encoding = Some(new_encoding);
                    path = Some((new_path, new_path_size));
                } else {
                    break;
                }
                println!();
            }
        }
        SpeedSetting::Instant => {
            println!("Speed setting is instant, skipping optimal encoding search");

            encoding = Some(seed_encoding);
            path = Some((seed_path, seed_path_size));
        }
    }
    println!();

    let encoding = encoding.unwrap();

    println!("Best encoding:");
    println!("  {}", encoding);

    let path = path.unwrap().0;

    // Encode
    let output = encode(&input, &path, &encoding, |pos| {
        let percent_complete = pos * 100 / uncompressed_size;
        print!("\rEncoding ... {}%", percent_complete);
        stdout().flush().expect("Couldn't flush stdout");
    });
    println!();
    println!("  Encoded size: {} bytes", output.len());
    println!();

    // Decompression check
    let decompressed = decompress(output.clone(), uncompressed_size, |pos| {
        let percent_complete = pos * 100 / uncompressed_size;
        print!("\rDecompression check ... {}%", percent_complete);
        stdout().flush().expect("Couldn't flush stdout");
    });

    if input != decompressed {
        println!("");
        panic!("Input/decompressed don't match");
    }
    println!("\rDecompression check OK!     ");
    println!();

    // Write output file
    File::create(&output_file_name).expect("Couldn't create output file").write_all(&output).expect("Couldn't write output file");

    println!("Output written to {}", output_file_name);

    // Write report
    generate_report(&report_file_name, &input, &path, &encoding);

    println!("Report written to {}", report_file_name);
    println!();

    // Final stats
    let total_duration = total_start_time.elapsed();
    let total_elapsed_time = (total_duration.as_secs() as f64) + (total_duration.subsec_nanos() as f64) / 1.0e9;

    let total_compressed_size = output.len();

    let total_ratio = (1.0 - (total_compressed_size as f64) / (uncompressed_size as f64)) * 100.0;

    println!("Final results:");
    println!("  {} -> {} bytes ({:.*}%)", uncompressed_size, total_compressed_size, 2, total_ratio);
    println!("  completed in {:.*}s", 2, total_elapsed_time);

    println!();
}

fn compress_and_print_stats(edge_lists: &Vec<Vec<Edge>>, encoding: &Encoding, uncompressed_size: usize, action_name: &str) -> (Vec<Edge>, u32) {
    let compress_start_time = Instant::now();

    let edges = compress(&edge_lists, encoding, uncompressed_size, |pos| {
        let percent_complete = pos * 100 / uncompressed_size;
        print!("\r{} ... {}%", action_name, percent_complete);
        stdout().flush().expect("Couldn't flush stdout");
    });
    println!();

    let compress_duration = compress_start_time.elapsed();
    let compress_elapsed_time = (compress_duration.as_secs() as f64) + (compress_duration.subsec_nanos() as f64) / 1.0e9;
    let compress_data_rate_kb_s = (uncompressed_size as f64) / 1024.0 / (compress_elapsed_time as f64);

    let compressed_size_bits = edges.iter().fold(0, |acc, edge| acc + encoding.bit_length(edge));
    let compressed_size_bytes = compressed_size_bits / 8;

    let ratio = (1.0 - (compressed_size_bytes as f64) / (uncompressed_size as f64)) * 100.0;

    println!("  {} -> ~{} bytes ({} bits, {:.*}%)", uncompressed_size, compressed_size_bytes, compressed_size_bits, 2, ratio);
    println!("  completed in {:.*}s (~{:.*}kb/s)", 2, compress_elapsed_time, 2, compress_data_rate_kb_s);

    (edges, compressed_size_bits)
}

#[derive(Clone, Copy)]
enum Edge {
    Literal { byte: u8 },
    Backreference { distance: u32, length: u32 },
}

impl Edge {
    fn byte_length(&self) -> u32 {
        match self {
            &Edge::Literal { .. } => 1,
            &Edge::Backreference { length, .. } => length,
        }
    }
}

trait Encoding {
    fn bit_length(&self, edge: &Edge) -> u32;
}

#[derive(Debug, Clone)]
struct TableEncodingEntry {
    min: u32,
    max: u32,
    num_additional_bits: u32,
}

struct TableEncoding {
    distance_table_1: Vec<TableEncodingEntry>,
    distance_table_2: Vec<TableEncodingEntry>,
    length_table: Vec<TableEncodingEntry>,
}

struct TableEncodingSearchNode<'a> {
    entry: TableEncodingEntry,
    total_bits: u32,
    next: Option<&'a TableEncodingSearchNode<'a>>,
}

impl TableEncoding {
    fn new(path: &Vec<Edge>) -> TableEncoding {
        // Get stats
        let mut distance_stats_1 = BTreeMap::new();
        let mut distance_stats_2 = BTreeMap::new();
        let mut length_stats = BTreeMap::new();

        for edge in path.iter() {
            if let &Edge::Backreference { distance, length } = edge {
                if length == 1 {
                    *distance_stats_1.entry(distance).or_insert(0) += 1;
                } else {
                    *distance_stats_2.entry(distance).or_insert(0) += 1;
                }
                *length_stats.entry(length).or_insert(0) += 1;
            }
        }

        // Ensure max values are represented in stats
        //  Technically this shouldn't be necessary to find an encoding for a given set of edges,
        //  but since we'll use this encoding to compress the stream again and we may find a new
        //  edge path that uses different lengths, and we want to be sure we can encode all
        //  possible values.
        // TODO: Replace magic numbers with the same max constants the search uses
        distance_stats_1.entry(65535).or_insert(1);
        distance_stats_2.entry(65535).or_insert(1);
        length_stats.entry(65535).or_insert(1);

        // Turn stats into ordered lists for convenience while searching
        let distance_stats_1 = distance_stats_1.into_iter().collect::<Vec<_>>();
        let distance_stats_2 = distance_stats_2.into_iter().collect::<Vec<_>>();
        let length_stats = length_stats.into_iter().collect::<Vec<_>>();

        // Perform searches
        TableEncoding {
            distance_table_1: TableEncoding::find_best_table(&distance_stats_1),
            distance_table_2: TableEncoding::find_best_table(&distance_stats_2),
            length_table: TableEncoding::find_best_table(&length_stats),
        }
    }

    fn find_best_table(stats: &[(u32, u32)]) -> Vec<TableEncodingEntry> {
        // Transform stats into a reverse running total over all possible values
        //  This eliminates a lot of redundant calculations during the search
        let mut reverse_running_total = vec![0; 65537];
        for &(value, count) in stats.iter() {
            reverse_running_total[value as usize] = count;
        }
        let mut acc = 0;
        let mut i = 65536;
        loop {
            acc += reverse_running_total[i];
            reverse_running_total[i] = acc;

            if i == 0 {
                break;
            }
            i -= 1;
        }

        let arena = Arena::new();
        // TODO: Replace magic numbers for min with the same min constants the search uses
        let mut head = TableEncoding::find_best_tail(0, 1, &reverse_running_total, &arena, &mut FnvHashMap::with_capacity_and_hasher(1024 * 512, Default::default()));
        let mut table = Vec::with_capacity(16);
        let mut min = 1;
        for _ in 0..16 {
            if let Some(node) = head {
                let entry = node.entry.clone();
                min = entry.max;
                table.push(entry);
                head = node.next;
            } else {
                let max = min + 1;
                table.push(TableEncodingEntry {
                    min: min,
                    max: max,
                    num_additional_bits: 0,
                });
                min = max;
            }
        }
        table
    }

    fn find_best_tail<'a>(
        level: u32,
        min: u32,
        reverse_running_total: &[u32],
        arena: &'a Arena<TableEncodingSearchNode<'a>>,
        cache: &mut FnvHashMap<(u32, u32), Option<&'a TableEncodingSearchNode<'a>>>) -> Option<&'a TableEncodingSearchNode<'a>> {
        // Early-out if we've already calculated and cached the result for these param's
        let cache_key = (level, min);
        if let Some(tail) = cache.get(&cache_key) {
            return tail.clone();
        }

        let mut best_tail: Option<&'a TableEncodingSearchNode> = None;

        let num_prefix_bits = unary_bit_length(level);
        let min_index = if min > 65536 { 65536 } else { min } as usize;

        // Test different bit lengths for current entry
        for num_additional_bits in 0..16 {
            // Find range of numbers we can cover for a particular number of bits
            let max = min + (1 << num_additional_bits);
            let max_index = if max > 65536 { 65536 } else { max } as usize;

            // If we're at the last level but can't encode the last value, we've reached the end of an invalid tail,
            //  so we need to reject it and try adding more bits
            if level == 15 && reverse_running_total[max_index] != 0 {
                continue;
            }

            // Calculate cost to encode the range we're testing
            let num_values = reverse_running_total[min_index] - reverse_running_total[max_index];
            let num_bits_per_value = num_prefix_bits + num_additional_bits;
            let mut total_bits = num_values * num_bits_per_value;

            // If this configuration is already not going to be cheaper than the current best, we can early-out
            //  here before testing potential tails and additional bits, as those are only going to make it even
            //  more expensive
            if best_tail.map(|best_tail| total_bits >= best_tail.total_bits).unwrap_or(false) {
                break;
            }

            // If we're not at the end of the table or the range of numbers, find the best available tail, and add
            //  its cost to the tail we're testing
            let tail = if level < 15 && reverse_running_total[max_index] != 0 {
                let tail = TableEncoding::find_best_tail(level + 1, max, reverse_running_total, arena, cache);
                if let Some(tail_node) = tail {
                    total_bits += tail_node.total_bits;
                    tail
                } else {
                    // If we couldn't find a suitable tail, then we're not going to be able to encode all possible
                    //  values, so we need to skip this iteration
                    continue;
                }
            } else {
                None
            };

            // Final test for this configuration
            if best_tail.map(|best_tail| total_bits < best_tail.total_bits).unwrap_or(true) {
                // We passed, make a new head for this tail
                let head = arena.alloc(TableEncodingSearchNode {
                    entry: TableEncodingEntry {
                        min: min,
                        max: max,
                        num_additional_bits: num_additional_bits,
                    },
                    total_bits: total_bits,
                    next: tail,
                });
                best_tail = Some(head);
            }
        }

        // Insert best entry into cache
        cache.insert(cache_key, best_tail);

        best_tail
    }

    fn distance_entry_1(&self, value: u32) -> Option<(&TableEncodingEntry, usize)> {
        TableEncoding::search_table(&self.distance_table_1, value)
    }

    fn distance_entry_2(&self, value: u32) -> Option<(&TableEncodingEntry, usize)> {
        TableEncoding::search_table(&self.distance_table_2, value)
    }

    fn length_entry(&self, value: u32) -> Option<(&TableEncodingEntry, usize)> {
        TableEncoding::search_table(&self.length_table, value)
    }

    fn search_table(table: &Vec<TableEncodingEntry>, value: u32) -> Option<(&TableEncodingEntry, usize)> {
        match table.binary_search_by(|entry| {
            if entry.max <= value {
                Ordering::Less
            } else if entry.min > value {
                Ordering::Greater
            } else {
                Ordering::Equal
            }
        }) {
            Ok(index) => Some((&table[index], index)),
            _ => None
        }
    }
}

impl Default for TableEncoding {
    // The default TableEncoding is simply one of the results from the optimal search at some
    //  point. It's not guaranteed to be optimal for any source, but it's trivial to construct
    //  and useful for having some encoding when a path isn't known, and for extremely fast
    //  compression where optimal ratio isn't as important.
    fn default() -> TableEncoding {
        let distance_1_encoding = "200056789abcdef5";
        let distance_2_encoding = "456858abc9839efd";
        let length_encoding = "112335639c686efe";

        TableEncoding {
            distance_table_1: decode_table(distance_1_encoding),
            distance_table_2: decode_table(distance_2_encoding),
            length_table: decode_table(length_encoding),
        }
    }
}

fn decode_table(encoding: &str) -> Vec<TableEncodingEntry> {
    let mut ret = Vec::with_capacity(16);
    let mut min = 1;
    for i in 0..16 {
        let c = encoding.chars().nth(i).unwrap() as u32;
        let num_additional_bits = if c >= '0' as u32 && c <= '9' as u32 {
            c - ('0' as u32)
        } else {
            c - ('a' as u32) + 0x0a
        };
        let max = min + (1 << num_additional_bits);
        ret.push(TableEncodingEntry {
            min: min,
            max: max,
            num_additional_bits: num_additional_bits,
        });
        min = max;
    }
    ret
}

impl fmt::Display for TableEncoding {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for i in 0..16 {
            write!(f, "{:x}", self.distance_table_1[i].num_additional_bits)?;
        }
        write!(f, ",")?;
        for i in 0..16 {
            write!(f, "{:x}", self.distance_table_2[i].num_additional_bits)?;
        }
        write!(f, ",")?;
        for i in 0..16 {
            write!(f, "{:x}", self.length_table[i].num_additional_bits)?;
        }

        Ok(())
    }
}

impl Encoding for TableEncoding {
    fn bit_length(&self, edge: &Edge) -> u32 {
        match edge {
            &Edge::Literal { .. } => 1 + 8,
            &Edge::Backreference { distance, length } => {
                let (distance_entry, distance_entry_index) = if length == 1 { self.distance_entry_1(distance) } else { self.distance_entry_2(distance) }.unwrap();
                let distance_bit_length = unary_bit_length(distance_entry_index as _) + distance_entry.num_additional_bits;
                let (length_entry, length_entry_index) = self.length_entry(length).unwrap();
                let length_bit_length = unary_bit_length(length_entry_index as _) + length_entry.num_additional_bits;
                1 + distance_bit_length + length_bit_length
            }
        }
    }
}

fn unary_bit_length(x: u32) -> u32 {
    x + 1
}

fn parse<F>(input: Vec<u8>, speed_setting: SpeedSetting, f: F) -> Vec<Vec<Edge>> where F: Fn(usize) {
    f(0);

    let input_len = input.len();

    let mut edge_lists = vec![Vec::new(); input_len];

    let mut match_dictionary = HashMap::new();
    let mut byte_index = 0;
    while byte_index < input_len {
        let edge_list = &mut edge_lists[byte_index];

        // Add literal edge
        edge_list.push(Edge::Literal { byte: input[byte_index] });

        // Search for match edges
        let max_distance = 65535;
        let max_length = 65535;
        let min_length = 1;

        // If we find a match with a length above a certain threshold, it's highly likely that it's with a long string of the
        //  same value over and over. Since this is our algorithm's degenerate case causing very large exponential execution
        //  times, in that situation we're just going to greedily take the match, rather than searching properly through it.
        //  This technically makes the parse potentially sub-optimal, but makes my test cases parse ~60x faster and produces
        //  nearly identical (+/-8b) results. Should look into removing this eventually and writing a smarter parser/matcher,
        //  but for now this should be a pretty good shortcut to enable faster iteration times while tweaking the compression/
        //  encoding.
        let min_greedy_len = match speed_setting {
            SpeedSetting::Optimal => 4096,
            SpeedSetting::Instant => 512,
        };

        // Update match dictionary
        if byte_index > max_distance {
            let remove_index = byte_index - max_distance - 1;
            match_dictionary.entry(input[remove_index]).or_insert(BTreeSet::new()).remove(&remove_index);
        }

        if byte_index >= 1 {
            let add_index = byte_index - 1;
            match_dictionary.entry(input[add_index]).or_insert(BTreeSet::new()).insert(add_index);
        }

        let mut longest_match_len = None;

        if byte_index >= 1 {
            let matches = match_dictionary.entry(input[byte_index]).or_insert(BTreeSet::new());
            for search_start_index in matches.iter().cloned() {
                let mut search_index = search_start_index + 1;
                let mut search_cmp_index = byte_index + 1;
                while search_index - search_start_index < max_length && search_cmp_index < input_len && input[search_index] == input[search_cmp_index] {
                    search_index += 1;
                    search_cmp_index += 1;
                }

                let length = search_index - search_start_index;
                if length >= min_length {
                    // Add match edge to list
                    edge_list.push(Edge::Backreference {
                        distance: (byte_index - search_start_index) as _,
                        length: (search_index - search_start_index) as _,
                    });

                    if longest_match_len.map(|x| length > x).unwrap_or(true) {
                        longest_match_len = Some(length);
                    }
                }
            }
        }

        if (byte_index & 0x3f) == 0 {
            f(byte_index);
        }

        byte_index += longest_match_len.map(|x| if x >= min_greedy_len { x } else { 1 }).unwrap_or(1);
    }

    f(input_len);

    edge_lists
}

#[derive(Default, Clone)]
struct Node {
    cost: Option<u32>,
    cost_src: Option<(usize, Edge)>,
}

fn compress<F>(edge_lists: &Vec<Vec<Edge>>, encoding: &Encoding, input_len: usize, f: F) -> Vec<Edge> where F: Fn(usize) {
    f(0);

    // Allocate nodes
    let mut nodes = vec![Node::default(); input_len + 1];

    // Ensure first node's cost is 0
    nodes[0].cost = Some(0);

    // For each node corresponding to a byte in the sequence, test edges from edge lists and uncompressed blocks, and relax node
    for byte_index in 0..input_len {
        let edge_list = &edge_lists[byte_index];

        // Skip this byte if it has an empty edge list
        if edge_list.is_empty() {
            continue;
        }

        // Test edges from edge list
        for edge in edge_list.iter().cloned() {
            compress_test_edge(edge, encoding, &mut nodes, byte_index);
        }

        if (byte_index & 0x3f) == 0 {
            f(byte_index);
        }
    }

    f(input_len);

    // Trace shortest path
    let mut path = Vec::new();
    let mut node_index = input_len;
    loop {
        let (next_node_index, edge) = nodes[node_index].cost_src.unwrap();
        path.push(edge);
        node_index = next_node_index;

        if node_index == 0 {
            break;
        }
    }
    path.reverse();

    path
}

fn compress_test_edge(edge: Edge, encoding: &Encoding, nodes: &mut Vec<Node>, src_node_index: usize) {
    let src_cost = nodes[src_node_index].cost.unwrap();
    let path_cost = src_cost + encoding.bit_length(&edge);

    let dst_node_index = src_node_index + (edge.byte_length() as usize);
    let dst_node = &mut nodes[dst_node_index];

    if dst_node.cost.map(|cost| path_cost < cost).unwrap_or(true) {
        dst_node.cost = Some(path_cost);
        dst_node.cost_src = Some((src_node_index, edge));
    }
}

struct EncoderState {
    output: Vec<u8>,

    bit_buffer: u32,
    bit_index: u32,
}

fn encode<F>(input: &[u8], path: &Vec<Edge>, encoding: &TableEncoding, f: F) -> Vec<u8> where F: Fn(usize) {
    f(0);

    let input_len = input.len();

    let mut state = EncoderState {
        output: Vec::new(),

        bit_buffer: 0,
        bit_index: 0,
    };

    // Output encoding table entries
    for i in 0..16 {
        encode_write_n_bit_value(&mut state, 4, encoding.length_table[i].num_additional_bits);
    }
    for i in 0..16 {
        encode_write_n_bit_value(&mut state, 4, encoding.distance_table_2[i].num_additional_bits);
    }
    for i in 0..16 {
        encode_write_n_bit_value(&mut state, 4, encoding.distance_table_1[i].num_additional_bits);
    }

    let mut byte_index = 0;
    for edge in path.iter() {
        match edge {
            &Edge::Literal { byte } => {
                encode_write_bit(&mut state, 0);
                encode_write_byte(&mut state, byte as _);
                byte_index += 1;
            }
            &Edge::Backreference { distance, length } => {
                encode_write_bit(&mut state, 1);

                let (length_entry, length_entry_index) = encoding.length_entry(length).unwrap();
                encode_write_unary(&mut state, length_entry_index as _);
                encode_write_n_bit_value(&mut state, length_entry.num_additional_bits, length - length_entry.min);

                let (distance_entry, distance_entry_index) = if length == 1 { encoding.distance_entry_1(distance) } else { encoding.distance_entry_2(distance) }.unwrap();
                encode_write_unary(&mut state, distance_entry_index as _);
                encode_write_n_bit_value(&mut state, distance_entry.num_additional_bits, distance - distance_entry.min);

                byte_index += length as usize;
            }
        }

        if (byte_index & 0x3f) == 0 {
            f(byte_index);
        }
    }

    encode_flush(&mut state);

    f(input_len);

    state.output
}

fn encode_write_bit(state: &mut EncoderState, bit: u32) {
    encode_push_buffer_bit(state, bit);

    if state.bit_index == 8 {
        encode_flush(state);
    }
}

fn encode_write_byte(state: &mut EncoderState, byte: u32) {
    for i in 0..8 {
        // Write msb -> lsb
        encode_write_bit(state, (byte >> (7 - i)) & 1);
    }
}

fn encode_write_unary(state: &mut EncoderState, n: u32) {
    for _ in 0..n {
        encode_write_bit(state, 0);
    }
    encode_write_bit(state, 1);
}

fn encode_write_n_bit_value(state: &mut EncoderState, n: u32, x: u32) {
    for digit_index in 0..n {
        // Write msb -> lsb
        encode_write_bit(state, (x >> (n - 1 - digit_index)) & 1);
    }
}

fn encode_flush(state: &mut EncoderState) {
    if state.bit_index == 0 {
        return;
    }

    while state.bit_index < 8 {
        encode_push_buffer_bit(state, 0);
    }

    state.output.push(state.bit_buffer as _);

    state.bit_buffer = 0;
    state.bit_index = 0;
}

fn encode_push_buffer_bit(state: &mut EncoderState, bit: u32) {
    state.bit_buffer = (state.bit_buffer >> 1) | (bit << 7);
    state.bit_index += 1;
}

struct DecompressorState {
    input: Vec<u8>,
    output: Vec<u8>,

    input_index: usize,
    bit_buffer: u32,
    bit_index: u32,
}

fn decompress<F>(input: Vec<u8>, uncompressed_size: usize, f: F) -> Vec<u8> where F: Fn(usize) {
    let mut state = DecompressorState {
        input: input,
        output: Vec::new(),

        input_index: 0,
        bit_buffer: 0,
        bit_index: 0,
    };

    f(0);

    // Decode encoding table entries
    let mut table = Vec::with_capacity(48);
    let mut min = 0;
    for i in 0..48 {
        if (i & 0x0f) == 0 {
            min = 1;
        }
        let num_additional_bits = decompress_read_n_bit_value(&mut state, 4);
        table.push((min, num_additional_bits));
        min += 1 << num_additional_bits;
    }

    while state.output.len() < uncompressed_size {
        match decompress_read_bit(&mut state) {
            1 => {
                // Backreference
                let length_index = decompress_read_unary(&mut state);
                let ref length_entry = table[length_index as usize];
                let length = length_entry.0 + decompress_read_n_bit_value(&mut state, length_entry.1);

                let mut distance_index = 16 + decompress_read_unary(&mut state);
                if length == 1 {
                    distance_index += 16;
                }
                let ref distance_entry = table[distance_index as usize];
                let distance = distance_entry.0 + decompress_read_n_bit_value(&mut state, distance_entry.1);

                let mut index = state.output.len() - (distance as usize);
                for _ in 0..length {
                    let byte = state.output[index];
                    decompress_write_byte(&mut state, byte as _);
                    index += 1;
                }
            }
            _ => {
                // Literal
                let byte = decompress_read_byte(&mut state);
                decompress_write_byte(&mut state, byte);
            }
        }

        if (state.output.len() & 0x3f) == 0 {
            f(state.output.len());
        }
    }

    f(uncompressed_size);

    state.output
}

fn decompress_read_bit(state: &mut DecompressorState) -> u32 {
    if state.bit_index == 0 {
        state.bit_buffer = state.input[state.input_index] as _;
        state.input_index += 1;
    }

    let ret = state.bit_buffer & 1;
    state.bit_buffer >>= 1;

    state.bit_index = (state.bit_index + 1) & 7;

    ret
}

fn decompress_read_byte(state: &mut DecompressorState) -> u32 {
    let mut ret = 0;

    for _ in 0..8 {
        // Read msb -> lsb
        ret <<= 1;
        ret |= decompress_read_bit(state);
    }

    ret
}

fn decompress_read_unary(state: &mut DecompressorState) -> u32 {
    let mut n = 0;
    while decompress_read_bit(state) == 0 {
        n += 1;
    }

    n
}

fn decompress_read_n_bit_value(state: &mut DecompressorState, n: u32) -> u32 {
    let mut x = 0;
    for _ in 0..n {
        // Read msb -> lsb
        x <<= 1;
        x |= decompress_read_bit(state);
    }

    x
}

fn decompress_write_byte(state: &mut DecompressorState, byte: u32) {
    state.output.push(byte as _);
}

fn generate_report<P: AsRef<Path>>(output_file_name: P, input: &[u8], edges: &[Edge], encoding: &Encoding) {
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

    let mut num_literal_edges = 0;
    let mut num_literal_bits = 0;
    let mut num_backreference_edges = 0;
    let mut num_backreference_bits = 0;

    let mut backreference_distance_counts = BTreeMap::new();
    let mut backreference_length_counts = BTreeMap::new();

    for edge in edges.iter() {
        let compressed_bits = encoding.bit_length(edge);

        match edge {
            &Edge::Literal { .. } => {
                num_literal_edges += 1;
                num_literal_bits += compressed_bits;
            }
            &Edge::Backreference { distance, length } => {
                num_backreference_edges += 1;
                num_backreference_bits += compressed_bits;

                *backreference_distance_counts.entry(distance).or_insert(0) += 1;
                *backreference_length_counts.entry(length).or_insert(0) += 1;
            }
        }
    }

    write!(output, "<div>
        <h3>overview</h3>").unwrap();

    write!(output, "<p>edge totals</p>
        <table>
            <tr>
                <th>edge type</th>
                <th>count</th>
                <th>count%</th>
                <th>total comp. bits</th>
                <th>total comp. bytes</th>
            </tr>
            <tr>
                <td>Literal</td>
                <td>{}</td>
                <td>{:.*}%</td>
                <td>{} bits</td>
                <td>~{} bytes</td>
            </tr>
            <tr>
                <td>Backreference</td>
                <td>{}</td>
                <td>{:.*}%</td>
                <td>{} bits</td>
                <td>~{} bytes</td>
            </tr>
        </table>",
        num_literal_edges, 2, (num_literal_edges as f64) / (edges.len() as f64) * 100.0, num_literal_bits, num_literal_bits / 8,
        num_backreference_edges, 2, (num_backreference_edges as f64) / (edges.len() as f64) * 100.0, num_backreference_bits, num_backreference_bits / 8).unwrap();

    write!(output, "</div>").unwrap();

    write!(output, "<div>
        <h3>key</h3>
        <p>").unwrap();
    for i in 0..10 {
        if i < 8 {
            write!(output, "<span class=\"heatmap-{}\">00</span> {}-{} bits/byte<br />", i, i, i + 1).unwrap();
        } else {
            write!(output, "<span class=\"heatmap-{}\">00</span> &nbsp;&nbsp;{} bits/byte<br />", i, i).unwrap();
        }
    }

    write!(output, "</p>
    </div>").unwrap();

    write!(output, "<div>
        <h3>heatmap</h3>
        <p>").unwrap();

    let mut byte_index = 0;
    for edge in edges {
        let num_bytes = edge.byte_length();
        let uncompressed_bits = num_bytes * 8;
        let compressed_bits = encoding.bit_length(edge);
        let byte_size = (8.0 * (compressed_bits as f64) / (uncompressed_bits as f64)).round() as u32;

        let title = match edge {
            &Edge::Literal { byte } => format!("literal (byte: 0x{:02x})", byte),
            &Edge::Backreference { distance, length } => format!("backreference (distance: {}, length: {})", distance, length),
        };
        let start_span = format!("<span title=\"{}\ntotal comp. bits: {}\" class=\"heatmap-{}\">", title, encoding.bit_length(edge), byte_size);
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

    /*write!(output, "<div>").unwrap();
    write!(output, "<h3>backreference distance distribution</h3>
        <table>
            <tr>
                <th>distance</th>
                <th>count</th>
                <th>count%</th>
                <th>coded bits</th>
                <th>total bits</th>
                <th>total bytes</th>
                <th>histogram ((count + 1) / 2)</th>
            </tr>").unwrap();
    for (distance, count) in backreference_distance_counts.clone().into_iter() {
        write!(output, "<tr>
                <td>{}</td>
                <td>{}</td>
                <td>{:.*}%</td>
                <td>{}</td>
                <td>{}</td>
                <td>~{}</td>", distance, count, 2, (count as f64) / (num_backreference_edges as f64) * 100.0, gamma_bit_length(distance), gamma_bit_length(distance) * count, gamma_bit_length(distance) * count / 8).unwrap();
        write!(output, "<td>").unwrap();
        for _ in 0..(count + 1) / 2 {
            write!(output, "*").unwrap();
        }
        write!(output, "</td>").unwrap();
        write!(output, "</tr>").unwrap();
    }
    write!(output, "</table>").unwrap();
    write!(output, "</div>").unwrap();

    write!(output, "<div>").unwrap();
    write!(output, "<h3>backreference length distribution</h3>
        <table>
            <tr>
                <th>length</th>
                <th>count</th>
                <th>count%</th>
                <th>coded bits</th>
                <th>total bits</th>
                <th>total bytes</th>
                <th>histogram ((count + 1) / 2)</th>
            </tr>").unwrap();
    for (length, count) in backreference_length_counts.clone().into_iter() {
        let coded_bits = encoding.bit_length()
        write!(output, "<tr>
                <td>{}</td>
                <td>{}</td>
                <td>{:.*}%</td>
                <td>{}</td>
                <td>{}</td>
                <td>~{}</td>", length, count, 2, (count as f64) / (num_backreference_edges as f64) * 100.0, gamma_bit_length(length), gamma_bit_length(length) * count, gamma_bit_length(length) * count / 8).unwrap();
        write!(output, "<td>").unwrap();
        for _ in 0..(count + 1) / 2 {
            write!(output, "*").unwrap();
        }
        write!(output, "</td>").unwrap();
        write!(output, "</tr>").unwrap();
    }
    write!(output, "</table>").unwrap();
    write!(output, "</div>").unwrap();*/

    write!(output, "</body>
</html>").unwrap();
}
