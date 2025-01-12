#[derive(Debug)]
struct Operand(i32);

#[derive(Debug)]
struct ComboOperand(Operand);

#[derive(Debug)]
enum Instr {
  Adv(ComboOperand),
  Bxl(Operand),
  Bst(ComboOperand),
  Jnz(Operand),
  Bxc,
  Out(ComboOperand),
  Bdv(ComboOperand),
  Cdv(ComboOperand),
}

impl Instr {
  fn encode(value: i32, op: i32) -> Self {
    match value {
      0 => Self::Adv(ComboOperand::new(op)),
      1 => Self::Bxl(Operand::new(op)),
      2 => Self::Bst(ComboOperand::new(op)),
      3 => Self::Jnz(Operand::new(op)),
      4 => Self::Bxc,
      5 => Self::Out(ComboOperand::new(op)),
      6 => Self::Bdv(ComboOperand::new(op)),
      7 => Self::Cdv(ComboOperand::new(op)),
      _ => panic!("Invalid opcode"),
    }
  }
}

impl Operand {
  fn new(value: i32) -> Self {
    if value / 8 != 0 {
      panic!("Operand must be a 3-bit value, got {value}");
    }

    Self(value)
  }

  fn get(&self) -> i32 {
    self.0
  }
}

impl ComboOperand {
  fn new(value: i32) -> ComboOperand {
    ComboOperand(Operand::new(value))
  }

  fn decode(&self, state: &Computer) -> i32 {
    let op = self.0.get();

    match op {
      0..=3 => op,
      4 => state.reg_a,
      5 => state.reg_b,
      6 => state.reg_c,
      _ => panic!("Invalid operand: {}", op),
    }
  }
}

#[derive(Debug)]
struct Computer {
  reg_a: i32,
  reg_b: i32,
  reg_c: i32,
  tape: Vec<Instr>,
  ip: usize,
  output: String,
}

fn division(num: i32, den: i32) -> i32 {
  if den < 0 {
    println!("Warning: casting {den} to u32, result: {}", (den as u32));
  }

  let res = num / (2 as i32).pow(den as u32);
  res
}

impl Computer {
  fn new(reg_a: i32, reg_b: i32, reg_c: i32, input: Vec<i32>) -> Self {
    let output = String::new();

    if input.len() % 2 != 0 {
      panic!("Odd length: last instruction is missing an operand")
    }

    let mut tape: Vec<Instr> = Vec::new();

    for chunk in input.chunks(2) {
      tape.push(Instr::encode(chunk[0], chunk[1]));
    }

    let ip = 0;
    Self {
      reg_a,
      reg_b,
      reg_c,
      output,
      ip,
      tape,
    }
  }

  fn exec(&mut self) {
    while let Some(instr) = self.tape.get(self.ip) {
      self.ip += 1;

      // state.exec(instr); // I don't get it

      println!("* Running {instr:?}");
      match instr {
        Instr::Adv(op) => {
          println!(
            "Numerator: {}, Denominator: {}",
            self.reg_a,
            op.decode(&self)
          );
          self.reg_a = division(self.reg_a, op.decode(&self));
        }
        Instr::Bdv(op) => {
          self.reg_b = division(self.reg_a, op.decode(&self));
        }
        Instr::Cdv(op) => {
          self.reg_c = division(self.reg_a, op.decode(&self));
        }
        Instr::Bxl(op) => {
          self.reg_b ^= op.get() as i32;
        }
        Instr::Bst(op) => self.reg_b = op.decode(&self) % 8,
        Instr::Jnz(op) => {
          if self.reg_a != 0 {
            self.ip = op.get() as usize
          }
        }
        Instr::Bxc => self.reg_b ^= self.reg_c,
        Instr::Out(op) => {
          let out = op.decode(&self) % 8;

          if !self.output.is_empty() {
            self.output.push(',');
          }

          self.output.push_str(&out.to_string());
        }
      }

      println!("{self:?}");
    }
  }
}

fn main() {
  let mut state = Computer::new(
    61657405,
    0,
    0,
    vec![2, 4, 1, 2, 7, 5, 4, 3, 0, 3, 1, 7, 5, 5, 3, 0],
  );

  state.exec();
  println!("{}", state.output);
}

#[test]
fn test_0() {
  let mut c = Computer::new(729, 0, 0, vec![0, 1, 5, 4, 3, 0]); // adv 1; out A; jnz 0
  c.exec();
  assert_eq!(c.output, String::from("4,6,3,5,6,3,5,2,1,0"));
}

#[test]
fn test_1() {
  let mut c = Computer::new(0, 0, 9, vec![2, 6]);
  c.exec();
  assert_eq!(c.reg_b, 1);
}

#[test]
fn test_2() {
  let mut c = Computer::new(10, 0, 0, vec![5, 0, 5, 1, 5, 4]);
  c.exec();
  assert_eq!(c.output, String::from("0,1,2"));
}
#[test]
fn test_3() {
  let mut c = Computer::new(2024, 0, 0, vec![0, 1, 5, 4, 3, 0]);
  c.exec();
  assert_eq!(c.output, String::from("4,2,5,6,7,7,7,7,3,1,0"));
  assert_eq!(c.reg_a, 0);
}

#[test]
fn test_4() {
  let mut c = Computer::new(0, 29, 0, vec![1, 7]);
  c.exec();
  assert_eq!(c.reg_b, 26);
}

#[test]
fn test_5() {
  let mut c = Computer::new(0, 2024, 43690, vec![4, 0]);
  c.exec();
  assert_eq!(c.reg_b, 44354);
}
