pub enum Code {
  Line(String),
  Block {
    indent: usize,
    inline: bool,
    start: String,
    body: Vec<Code>,
    end: String,
  },
}

impl From<Code> for Vec<String> {
  fn from(code: Code) -> Self {
    match code {
      Code::Line(line) => vec![line],
      Code::Block {
        indent,
        inline,
        start,
        body,
        end,
      } => {
        let mut lines = vec![start];
        if inline {
          for code in body {
            let sub_lines: Vec<String> = code.into();
            let last = lines.last_mut().unwrap();
            last.push(' ');
            last.push_str(sub_lines.first().unwrap());
            for line in sub_lines.into_iter().skip(1) {
              lines.push(format!("{}{}", " ".repeat(indent), line));
            }
          }
          let last = lines.last_mut().unwrap();
          last.push(' ');
          last.push_str(&end);
        } else {
          for code in body {
            let sub_lines: Vec<String> = code.into();
            for line in sub_lines {
              lines.push(format!("{}{}", " ".repeat(indent), line));
            }
          }
          lines.push(end);
        }
        lines
      }
    }
  }
}

impl From<Code> for String {
  fn from(code: Code) -> Self {
    let lines: Vec<String> = code.into();
    lines.join("\n")
  }
}
