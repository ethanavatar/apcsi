# apcsi

An interpreter for the College Board's AP Computer Science Principles pseudocode language.

The language specification can be found [here](https://apcentral.collegeboard.org/media/pdf/ap-computer-science-principles-exam-reference-sheet.pdf).

## To-Do

This tool is VERY much not complete and VERY buggy. Here's a list of things that need to be done:

- [ ] Functions with parameters
- [ ] Functions with return values
- [ ] `FOR EACH` loops
- [ ] Fix misc syntax errors where there shouldn't be any
- [ ] Fix misc runtime errors where there shouldn't be any
- [ ] Properly catch, recover, and report runtime errors (currently, the interpreter just crashes in-place)

## Installation

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
