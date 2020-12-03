use std::fs;
use std::str::FromStr;

#[derive(Debug)]
struct Password {
    lower: i32,
    upper: i32,
    letter: char,
    password: String,
}

impl FromStr for Password {
  type Err = std::convert::Infallible;

  fn from_str(input: &str) -> Result<Self, Self::Err> {
    let v: Vec<_> = input
      .split(|c| c == ':' || c == ' ' || c== '-')
      .filter(|s| !s.is_empty())
      .collect();
    Ok(Password {
      lower: v[0].parse().unwrap(),
      upper: v[1].parse().unwrap(),
      letter: v[2].to_owned().chars().nth(0).unwrap(),
      password: v[3].to_owned(),
    })
  }
}

fn main() {
    println!("[Part 1] {}", part_1(get_input()));
    println!("[Part 2] {}", part_2(get_input()));
}

fn get_input() -> Vec<Password> {
  fs::read_to_string("./input.txt")
    .unwrap()
    .trim()
    .split("\n")
    .map(|pw| Password::from_str(pw).unwrap())
    .collect()
}

fn part_1(pws: Vec<Password>) -> i32 {
  pws.iter()
    .filter(|p| is_valid_simple(p))
    .count() as i32
}

fn part_2(pws: Vec<Password>) -> i32 {
  pws.iter()
    .filter(|p| is_valid_complex(p))
    .count() as i32
}

fn is_valid_simple(pw: &Password) -> bool {
    let occurances = pw.password.chars().filter(|c| c == &pw.letter).count() as i32;
    occurances >= pw.lower && occurances <= pw.upper
}

fn is_valid_complex(pw: &Password) -> bool {
  let p0 = pw.password.chars().nth(pw.lower as usize - 1).map(|l| l == pw.letter);
  let p1 = pw.password.chars().nth(pw.upper as usize - 1).map(|l| l == pw.letter);

  match p0.zip(p1).map(|(x, y)| x ^ y) {
      Some(r) => r,
      None => false
  }
}