use std::fs;
use itertools::Itertools;

type Input = Vec<i32>;

fn main() {
  let expenses = get_input();
  println!("[Part 1 Slow] {}", part1_slow(expenses.clone()));
  println!("[Part 2 Slow] {}", part2_slow(expenses.clone()));
  println!("[Part 1 Fast] {}", part1_fast(expenses.clone()).unwrap());
  println!("[Part 2 Fast] {}", part2_fast(expenses.clone()).unwrap());
}

fn get_input() -> Input {
  fs::read_to_string("./input.txt")
    .unwrap()
    .trim()
    .split("\n")
    .map(|m| m.parse().unwrap())
    .collect()
}

fn part1_fast(items: Input) -> Option<i32> {
  for i in 0..items.len() {
    for j in 0..items.len() {
      if i != j && items[i] + items[j] == 2020 {
        return Some(items[i] * items[j]);
      }
    }
  }
  None
}

fn part2_fast(items: Input) -> Option<i32> {
  for i in 0..items.len() {
    for j in 0..items.len() {
      for k in 0..items.len() {
        if i == j || i == k || j == k {
          continue;
        }
        if items[i] + items[j] + items[k] == 2020 {
          return Some(items[i] * items[j] * items[k]);
        }
      }
    }
  }
  None
}

fn part1_slow(expenses: Input) -> i32 {
  find_product_of_2020(expenses.iter().combinations(2).collect())
}

fn part2_slow(expenses: Input) -> i32 {
  find_product_of_2020(expenses.iter().combinations(3).collect())
}

fn find_product_of_2020(vecs: Vec<Vec<&i32>>) -> i32 {
    vecs
      .iter()
      .map(|e| (e, e.iter().fold(0, |x, y| x + *y)))
      .filter(|(_, sum)| *sum == 2020)
      .map(|(e, _)| e.iter().fold(1, |x, y| x * *y))
      .last()
      .unwrap()
}