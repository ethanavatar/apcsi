# apcsi

An interpreter for the College Board's AP Computer Science Principles pseudocode language.

The language specification can be found [here](https://apcentral.collegeboard.org/media/pdf/ap-computer-science-principles-exam-reference-sheet.pdf).

## To-Do

This tool can be treated as untested and incomplete. Aside from general bugginess, these are the next things to do:

- [ ] Properly catch, recover, and report runtime errors (currently, the interpreter just crashes in-place without synchronization)
- [ ] Make more examples
- [ ] Add more tests

## Installation

Install using [Rust Cargo](https://www.rust-lang.org/tools/install):

```bash
$ git clone https://github.com/ethanavatar/apcsi.git
$ cd apcsi
$ cargo install --path .
...
```

## Usage

To interpret a file:

```bash
$ apcsi Hello.csp
Hello, Sailor!
```

To launch a REPL:

```bash
$ apcsi
>> DISPLAY "Hello, Sailor!"
Hello, Sailor!
```
