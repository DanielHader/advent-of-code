use std::collections::HashMap;
use std::fs::File;
use std::io::prelude::*;
use std::io::BufReader;

fn read_input(file_name: &str) -> (Vec<u32>, Vec<u32>) {
    let file = File::open(file_name).unwrap();
    let reader = BufReader::new(file);

    let mut v1 = Vec::<u32>::new();
    let mut v2 = Vec::<u32>::new();
    
    for line in reader.lines() {
        let nums: Vec<u32> = line.unwrap().split("   ").flat_map(|s| str::parse::<u32>(s)).collect();

        v1.push(nums[0]);
        v2.push(nums[1]);
    }
    
    (v1, v2)
}

fn main() {
    let (mut v1, mut v2) = read_input("input.txt");

    v1.sort();
    v2.sort();

    let mut dist = 0;
    
    for (a, b) in std::iter::zip(&v1, &v2) {
        dist += if a > b { a - b} else { b - a };
    }

    println!("distance = {dist}");

    let mut v2_counts = HashMap::<u32, u32>::new();
    for v in &v2 {
        if v2_counts.contains_key(&v) {
            *v2_counts.get_mut(v).unwrap() += 1;
        } else {
            v2_counts.insert(*v, 1);
        }
    }

    let mut sim = 0;
    for v in &v1 {
        if v2_counts.contains_key(&v) {
            sim += *v * v2_counts[v];
        }
    }

    println!("similarity = {sim}");
}
