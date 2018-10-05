use std::cmp;
use std::io;
use std::io::Write;
use std::str;

const TAB_WIDTH: usize = 2;

pub struct Writer<F: Write> {
    out: F,
    spaces: Vec<usize>,
    line_started: bool,
    line_length: usize,
    line_number: usize,
    max_line_length: usize,
}

impl<F: Write> Writer<F> {
    pub fn new(out: F) -> Writer<F> {
        Writer {
            out: out,
            spaces: vec![0],
            line_started: false,
            line_length: 0,
            line_number: 1,
            max_line_length: 0,
        }
    }

    fn spaces(&self) -> usize {
        *self.spaces.last().unwrap()
    }

    pub fn push_tab(&mut self) {
        let spaces = self.spaces() - (self.spaces() % TAB_WIDTH) + TAB_WIDTH;
        self.spaces.push(spaces);
    }

    pub fn pop_tab(&mut self) {
        assert!(!self.spaces.is_empty());
        self.spaces.pop();
    }

    pub fn new_line(&mut self) {
        write!(self.out, "\n").unwrap();
        self.line_started = false;
        self.line_length = 0;
        self.line_number += 1;
    }

    pub fn new_lines(&mut self, lines: u32) {
        for _ in 0..lines {
            self.new_line();
        }
    }

    pub fn open_brace(&mut self) {
        self.write(" {");
        self.push_tab();
        self.new_line();
    }

    pub fn close_brace(&mut self) {
        self.pop_tab();
        self.write("}");
    }

    pub fn write(&mut self, text: &'static str) {
        write!(self, "{}", text);
    }

    pub fn write_fmt(&mut self, fmt: ::std::fmt::Arguments) {
        FmtWriter(self).write_fmt(fmt).unwrap();
    }
}

struct FmtWriter<'a, F: 'a + Write>(&'a mut Writer<F>);

impl<'a, F: Write> Write for FmtWriter<'a, F> {
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        let writer = &mut self.0;

        let text = str::from_utf8(buf).unwrap();
        let mut first_line = true;

        for line in text.split(|x| x == '\n') {
            if !first_line {
                writeln!(writer.out, "")?;
                writer.line_started = false;
                writer.line_length = 0;
            }

            if !writer.line_started && !line.is_empty() {
                for _ in 0..writer.spaces() {
                    write!(writer.out, " ").unwrap();
                }
                writer.line_started = true;
                writer.line_length += writer.spaces();
            }
            write!(writer.out, "{}", line)?;

            writer.line_length += line.len();
            writer.max_line_length = cmp::max(writer.max_line_length, writer.line_length);

            first_line = false;
        }

        Ok(text.len())
    }

    fn flush(&mut self) -> io::Result<()> {
        self.0.out.flush()
    }
}
