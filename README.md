# apcsi

An interpreter for the College Board's AP Computer Science Principles pseudocode language.

The language specification can be found [here](https://apcentral.collegeboard.org/media/pdf/ap-computer-science-principles-exam-reference-sheet.pdf).

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
