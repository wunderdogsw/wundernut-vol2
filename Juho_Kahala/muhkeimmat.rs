#![feature(io,collections)]
use std::io::prelude::*;
use std::fs::File;
use std::collections::BitSet;
use std::env;


#[derive(Clone)]
struct Word<'a> {
    decomp: BitSet,
    word: &'a str,
}

impl<'a> Word<'a> {
    fn new(word: &str) -> Word {
        Word{ decomp: decompose(word), word: word }
    }
}

const ABC_LOWER: &'static str = "abcdefghijklmnopqrstuvwxyzåäö";
const ABC_UPPER: &'static str = "ABCDEFGHIJKLMNOPQRSTUVWXYZÅÄÖ";

fn decompose(word: &str) -> BitSet {
    word.chars()
        .filter_map(|c| ABC_LOWER.find(c).or_else(|| ABC_UPPER.find(c)))
        .collect::<BitSet>()
}

// store only maximal elements (in the partial order by inclusion)
fn lower_set_insert(set: &mut Vec<BitSet>, val: &BitSet) {
    if set.iter().any(|m| m.is_superset(val)) { return; }
    set.retain(|m| !m.is_subset(val));
    set.push(val.clone());
}

fn heft(k1: &BitSet, k2: &BitSet) -> usize {
    k1.union(k2).count()
}

struct HeftPair<'a> {
    a_b: BitSet,
    b_a: BitSet,
    anb: BitSet,
    words1: Vec<Word<'a>>,
    words2: Vec<Word<'a>>,
}

impl<'a> HeftPair<'a> {
    fn new(a: BitSet, b: BitSet) -> HeftPair<'a> {
        HeftPair{ a_b: a.difference(&b).collect(),
                  b_a: b.difference(&a).collect(),
                  anb: a.intersection(&b).collect(),
                  words1: Vec::new(), 
                  words2: Vec::new() }
    }

    fn add_matching(&mut self, word: &Word<'a>) {
        let tmp = word.decomp.difference(&self.anb).collect::<BitSet>();
        if tmp == self.a_b  {
            self.words1.push(word.clone());
        }
        if tmp == self.b_a  {
            self.words2.push(word.clone());
        }
    }
}


fn main() {
    let filename = match env::args().nth(1) {
        Some(x) => x,
        None => { println!("Usage: {} <file>", env::args().next().unwrap()); return; },
    };
    let file = match File::open(&filename) {
        Ok(x) => x,
        Err(e) => { println!("{}", e); return; },
    };
    let text = file.chars()
                   .map(|c| c.unwrap()) //panic on invalid utf8
                   .filter(|c| c.is_alphabetic() || c.is_whitespace()
                                                 || *c == '-'
                                                 || *c == '\'')
                   .collect::<String>();

// save words and their decompositions, and keep track of maximal decompositions
    let mut lower_set = Vec::<BitSet>::new();
    let word_list = text.split_whitespace()
                        .map(|s| Word::new(s))
                        .inspect(|w| lower_set_insert(&mut lower_set, &w.decomp))
                        .collect::<Vec<_>>();

// find the maximum heft
    let max_heft = lower_set.iter()
        .enumerate()
        .flat_map(|(i, v1)| std::iter::repeat(v1).zip(lower_set.iter().skip(i)))
        .map(|(v1, v2)| heft(v1, v2))
        .fold(0, |max, val| std::cmp::max(max, val));

// find pairs of decompositions that reach that maximum
    let mut heft_pairs = lower_set.iter()
        .enumerate()
        .flat_map(|(i, v1)| std::iter::repeat(v1).zip(lower_set.iter().skip(i)))
        .filter(|&(v1, v2)| heft(v1, v2) == max_heft)
        .map(|(v1, v2)| HeftPair::new(v1.clone(), v2.clone()))
        .collect::<Vec<_>>();

// find words that might be part of a maximal pairing
    for word in word_list.iter() {
        for hp in heft_pairs.iter_mut() {
            hp.add_matching(&word);
        }
    }

// collect the maximal pairs and sort each of them
    let mut word_pairs = heft_pairs.iter()
        .flat_map(|hp| std::iter::repeat(hp).zip(hp.words1.iter()))
        .flat_map(|(hp, w1)| std::iter::repeat(w1).zip(hp.words2.iter()))
        .filter(|&(w1,w2)| heft(&w1.decomp,&w2.decomp) == max_heft)
        .map(|(w1,w2)| (w1.word, w2.word))
        .map(|(w1,w2)| if w1 < w2 { (w1,w2) } else { (w2,w1) })
        .collect::<Vec<_>>();

// sort, remove duplicates, and print
    word_pairs.sort();
    word_pairs.dedup();
    println!("Suurin löydetty sanaparin muhkeus oli {}, jonka saavuttivat:", max_heft);
    for &(w1,w2) in word_pairs.iter() {
        println!("{}, {}", w1, w2);
    }
}
