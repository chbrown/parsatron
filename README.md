# Parsatron

[![Clojars Project](https://img.shields.io/clojars/v/chbrown/parsatron.svg)](https://clojars.org/chbrown/parsatron)
[![Travis CI Build Status](https://travis-ci.org/chbrown/parsatron.svg?branch=master)](https://travis-ci.org/chbrown/parsatron)
[![Coverage Status](https://coveralls.io/repos/github/chbrown/parsatron/badge.svg?branch=master)](https://coveralls.io/github/chbrown/parsatron?branch=master)


Born from Haskell's [Parsec](https://hackage.haskell.org/package/parsec) library, Parsatron is a functional parser library.
The Parsatron provides a lot of very small functions that can be combined into larger ones to very quickly write parsers for languages.

Like all parser combinator libraries, The Parsatron produces recursive-descent parsers that are best suited for LL(1) grammars.
However, The Parsatron offers *infinite lookahead* which means you can try and parse any insane thing you'd like and if it doesn't work out, fall back to where you started.
It's a feature that's worked out well for others.
I'm sure you'll find something useful to do with it.


## Installation

Releases are deployed to [Clojars](https://clojars.org/chbrown/parsatron).
Simply add

    [chbrown/parsatron "0.0.8"]

to your `project.clj` dependencies.


## ClojureScript Support

The Parsatron has been ported to ClojureScript and is included in the
library distribution. There are a few small differences between ClojureScript
and Clojure that follow The Parsatron into the browser, namely:

* The Parsatron makes liberal use of macros: `let->>` and `defparser` must be included via `:require-macros`
* ClojureScript has no notion of a character type like Clojure does.
  The Parsatron considers Strings of length 1 to be characters


## Usage

A basic syntax checker for a certain profane esoteric programming language could
be defined as follows:

    (defparser instruction []
      (choice (token= \>)
              (token= \<)
              (token= \+)
              (token= \-)
              (token= \.)
              (token= \,)
              (between (token= \[) (token= \]) (many (instruction)))))

    (defparser bf []
      (many (instruction))
      (eof))

The `defparser` forms create new parsers that you can combine into other, more
complex parsers. As you can see in this example, those parsers can be recursive.

The `choice`, `token`, `between` and `many` functions you see are themselves
combinators, provided *gratis* by the library. Some, like `choice`, `many`, and
`between`, take parsers as arguments and return you a new one, wholly different,
but exhibiting eerily familiar behavior. Some, like `token=`, take less exotic input
(in this case, a humble character) and return more basic parsers, that perform
what is asked of them without hesitation or spite.

You execute a parser over some input via the `run` form.

    (run (bf) ",>++++++[<-------->-],[<+>-]<.")

Currently, The Parsatron only provides character-oriented parsers, but the ideas
it's built on are powerful enough that with the right series of commits, it can
be made to run over sequence of arbitrary "tokens". Clojure's handling of
sequences and sequence-like things is a feature deeply ingrained in the language's
ethos. Look for expansion in this area.

Beyond just verifying that a string is a valid member of some language, The
Parsatron offers you facilities for interacting with and operating on the things
you parse via sequencing of multiple parsers and binding their results. The
macros `>>` and `let->>` embody this facility.

As an example, [bencoded strings](https://en.wikipedia.org/wiki/Bencode) are prefixed by their length and a colon:

    (defparser ben-string []
      (let->> [length (integer)]
        (>> (token= \:)
            (times length (character)))))

`let->>` allows you to capture and name the result of a parser so it's value may
be used later. `>>` is very similar to Clojure's `do` in that it executes it's
forms in order, but "throws away" all but the value of the last form.

    (run (ben-string) "4:spam") ;; => [\s \p \a \m]

## Testing

Tests are run on every commit via [Travis CI](https://travis-ci.org/chbrown/parsatron).

Testing on a local distribution:

* Run Clojure tests:

      lein test

* Run ClojureScript tests:

      lein doo rhino test once


### Coverage

* Calculate coverage of tests:

      lein cloverage

---

# Full Guide to the Parsatron

The Parsatron is a library for building parsers for languages.
For an overview of how it works internally you can watch [this talk](http://www.infoq.com/presentations/Parser-Combinators).

This document will show you the basics of how to use the Parsatron as an end user.

## Importing

Assuming you have the library installed, you can grab all the things you'll need by using it:

    (ns myparser.core
      (:require [parsatron.core :refer :all]))

## Running

Let's see how to run a basic parser.
It won't do much, but it will get something on the screen so we can try things as we go.
Assuming you've got everything imported:

    (run (token= \H) "Hello, world!")
    ; \H

The `run` function takes a parser and some input, runs the parser on that input, and returns the result.

The parser we passed here was `(token= \H)`.
We'll talk more about parsers in a second, but for now just know that it's a parser that will parse a single "H" character.

Notice that it only parsed the first character, and even though there was more left it still successfully returned.
We'll talk about how to make sure that there's no remaining input later.

## Input

We passed a string as the input to `run` in our first example, but the input doesn't necessarily have to be a string.
It can be any sequence. For example, this works:

    (run (token #{1 2}) [1 "cats" :dogs])
    ; 1

The `(token #{1 2})` is a parser that matches the *integer* 1 or the *integer* 2, and we've passed it a vector of things.

## Errors

If the parser you give to `run` can't parse the input successfully,
it will throw a `clojure.lang.ExceptionInfo` (which is a subclass of `java.lang.RuntimeException`):

    (run (token= \q) "Hello, world!")
    ; ExceptionInfo Unexpected token 'H' at line: 1 column: 1  [...]

The exception will tell you the line and column of the error, which is usually quite helpful.

## Parsers

Now that we've got the basics, it's time to talk about how to create new parsers.

A "parser" is, technically, a function that takes 5 arguments and returns a special value, but you don't need to worry about that yet.
What you *do* need to worry about is how to create them and combine them.

When we ran `(token= \H)` in the first example, it returned a parser.
`token=` itself is a *function* that, when given a character, creates a parser that parses that character.

Read that again and make sure you understand it before moving on.
`token=` is not a parser; it's a function that creates parsers.
Character goes in, parser comes out:

    (def h-parser (token= \h))
    (run h-parser "hi")
    ; \h

## Basic Built-In Parsers

There are a few other basic parser-creating functions that you'll probably find useful, which we'll talk about now.

### `token`

`token` creates parsers that match single items from the input stream (which are characters if the input stream happens to be a string).
You give it a predicate, and it returns a parser that parses and returns items that match the predicate.
For example:

    (defn less-than-five [i]
      (< i 5))

    (run (token less-than-five)
         [3])
    ; 3

The predicate can be any function, so things like anonymous functions and sets work well.

### `token=`

We've already seen `token=`, which creates parsers that parse and return a single, specific character.

    (run (token= \H) "Hello, world!")
    ; \H

### `character`

`character` creates parsers that will parse and return any character.
Remember that we can use the parsatron to parse more than just strings:

    (run (character) "Cats")
    ; \C

    (run (character) [\C \a \t \s])
    ; \C

    (run (character) [1 2 3])
    ; ExceptionInfo Unexpected token '1'  [...]

### `letter` and `digit`

`letter` and `digit` create parsers that parse and return letter characters (a-z and A-Z) and digit characters (0-9) respectively.

    (run (letter) "Dogs")
    ; \D

    (run (digit) "100")
    ; \1

Note that digit works with *character* objects.
It won't work with actual integers:

    (run (digit) [10 20 30])
    ; ClassCastException java.lang.Long cannot be cast to java.lang.Character  [...]

If you want a parser that matches numbers in a non-string input sequence, use `token` and the Clojure builtin function `number?` to make it:

    (run (token number?) [10 20 30])
    ; 10

### `string`

`string` creates parsers that parse and return a sequence of characters given as a string:

    (run (string "Hello") "Hello, world!")
    ; "Hello"

Note that this is the first time we've seen a parser that consumes more than one item in the input sequence.

### `eof`

`eof` creates parsers that ensure the input stream doesn't contain anything else:

    (run (eof) "")
    ; nil

    (run (eof) "a")
    ; ExceptionInfo Expected end of input at line: 1 column: 1  [..]

On its own it's not very useful, but we'll need it once we learn how to combine parsers.

### Combining Parsers

The Parsatron wouldn't be very useful if we could only ever parse one thing at a time.
There are a number of ways you can combine parsers to build up complex ones from basic parts.

### `>>`

The `>>` macro is the simplest way to combine parsers.
It takes any number of parsers and creates a new parser.
This new parser runs them in order and returns the value of the last one.

Again, `>>` takes *parsers* and returns a new *parser*.
We'll see this many times in this section.

Here's an example:

    (def my-parser (>> (token= \a) (digit)))

    (run my-parser "a5")
    ; \5

    (run my-parser "5a")
    ; ExceptionInfo Unexpected token '5' at line: 1 column: 1  [...]

    (run my-parser "b5")
    ; ExceptionInfo Unexpected token 'b' at line: 1 column: 1  [...]

    (run my-parser "aq")
    ; ExceptionInfo Unexpected token 'q' at line: 1 column: 2  [...]

We create a parser from two other parsers with `>>` and run it on some input.
`>>` runs its constituent parsers in order, and they all have to match for it to parse successfully.

Now that we can combine parsers, we can also ensure that there's no garbage after the stuff we parse by using `eof`:

    (run (>> (digit) (eof)) "1")
    ; nil

    (run (>> (digit) (eof)) "1 cat")
    ; ExceptionInfo Expected end of input at line: 1 column: 2  [...]

### `times`

The next way to combine parsers (or, really, a parser with itself) is the `times` function.

`times` is a function that takes a count and a parser, and returns a parser that repeats the one you gave it the specified number of times and returns the results concatenated into a sequence.

For example:

    (run (times 5 (letter)) "Hello, world!")
    ; (\H \e \l \l \o)

This is different than `(>> (letter) (letter) (letter) (letter) (letter))` because it returns *all* of the parsers' results, not just the last one.

### `many`

`many` is the first creator of "open-ended" parsers we've seen.
It's a function that takes a parser and returns a new parser that will parse zero or more of the one you gave it, and return the results concatenated into a sequence.

For example:

    (run (many (digit)) "100 cats")
    ; (\1 \0 \0)

Now we can start to build much more powerful parsers:

    (def number-parser (many (digit)))
    (def whitespace-parser (many (token #{\space \newline \tab})))

    (run (>> number-parser whitespace-parser number-parser) "100    400")
    ; (\4 \0 \0)

We still need to talk about how to get more than just the last return value, but that will come later.

### `many1`

`many1` is just like `many`, except that the parsers it creates require at least one item.
It's like `+` in a regular expression instead of `*`.

    (def number-parser (many (digit)))
    (def number-parser1 (many1 (digit)))

    (run number-parser "")
    ; []

    (run number-parser "100")
    ; (\1 \0 \0)

    (run number-parser1 "")
    ; ExceptionInfo Unexpected end of input at line: 1 column: 1  [...]

    (run number-parser1 "100")
    ; (\1 \0 \0)

### `either`

`either` is a parser that takes two parsers.
If the first one succeeds its value is returned, if it fails, the second parser is tried and it's value is returned.

    (def number (many1 (digit)))
    (def word   (many1 (letter)))
    (def number-or-word (either number word))

    (run number-or-word "dog")
    ; (\d \o \g)

    (run number-or-word "42")
    ; (\4 \2)

    (run number-or-word "@#$")
    ; ExceptionInfo Unexpected token '@', Unexpected token '@' at line: 1 column: 1  [...]

### `choice`

`choice` takes one or more parsers and creates a parser that will try each of them in order until one parses successfully, and return its result.
It is different from `either` in that it may take more than two parsers while `either` can only take 2.

For example:

    (def number (many1 (digit)))
    (def word (many1 (letter)))
    (def other (many1 (character)))

    (def number-or-word-or-anything (choice number word other))

    (run number-or-word-or-anything "dog")
    ; (\d \o \g)

    (run number-or-word-or-anything "42")
    ; (\4 \2)

    (run number-or-word-or-anything "!@#$")
    ; (\! \@ \# \$)

Notice that we used `many1` when defining the parsers `number` and `word`.
If we had used `many` then this would always parse as a number because if there were no digits it would successfully return an empty sequence.


### `between`

`between` is a function that takes three parsers, call them left, right, and center.
It creates a parser that parses them in left - center - right order and returns the result of center.

This is a convenient way to handle things like parentheses:

    (def whitespace-char (token #{\space \newline \tab}))
    (def optional-whitespace (many whitespace-char))

    (def open-paren (token= \())
    (def close-paren (token= \)))

    (def number (many1 (digit)))

    (run (between (>> open-paren optional-whitespace)
                  (>> optional-whitespace close-paren)
                  number)
        "(123    )")
    ; (\1 \2 \3)

This example is a bit more complicated than we've seen so far, so slow down and make sure you know what's going on.

The three parsers we're giving to `between` are:

1. `(>> open-paren optional-whitespace)`
2. `(>> optional-whitespace close-paren)`
3. `number`

Once you're comfortable with this example, it's time to move on to the next stage of parsing: building and returning values.

### Returning Values

So far we've looked at many ways to parse input.
If you just need to validate that input is in the correct format, but not *do* anything with it, you're all set.
But usually the goal of parsing something is to do things with it, so let's look at how that works now.

We've been using the word "returns" in a fast-and-loose fashion so far, but now it's time to look a bit more closely at what it means in the Parsatron.

### defparser and always

When we looked at parsers created with `token` (like `(token= \H)`) we said that these parsers *returned* that character they parsed.
That's not quite true. They actually return a specially-wrapped value.

If you want to know exactly what that special wrapping is, watch the [talk](http://www.infoq.com/presentations/Parser-Combinators).
But you don't really need to understand the guts to use the Parsatron.
You just need to know how to create them.

This is the first time we're going to be creating parsers that are more than just simple combinations of existing ones.
To do that we need to use a special macro that handles setting them up properly: `defparser`.
Look at the following example (don't worry about what `always` is yet):

    (defparser sample []
      (string "Hello")
      (always 42))

First of all, `defparser` doesn't define parsers.
It defines functions that *create* parsers, just like all of the ones we've seen so far.
Yes, I know how ridiculous that sounds. In practice it's only *slightly* confusing.

So now we've got a function `sample` that we can use to create a parser by calling it:

    (def my-sample-parser (sample))

Okay, now lets run it on some input:

    (run my-sample-parser "Hello, world!")
    ; 42

There's a bunch of interesting things going on here, so let's slow down and take a look.

First, the parsers created by the functions `defparser` defines implicitly wrap their bodies in `>>`, which as we've seen runs its argument parsers in order and returns the last result.
So our `(sample)` parser will run the "Hello" string parser, and then the always parser (which it uses as the result).

So what is this `always` thing? Well, remember at the beginning of this section we said that parsers return a specially-wrapped value? `always` is a way to simply stick a piece of data in this special wrapper so it can be the result of a parser.

Here's a little drawing that might help:

    raw input --> (run ...) --> raw output
                  |      ^
                  |      |
                  | wrapped output
                  v      |
               (some parser)

`run` takes the wrapped output from the parser and unwraps it for us before returning it, which is why our `run` calls always gave us vanilla Clojure data structures before.

We're almost to the point where we can create full-featured parsers.
The final piece of the puzzle is a way to intercept results and make decisions inside of our parsers.

### `let->>`

The `let->>` macro is the magic glue that's going to make creating your parsers fun.
In a nutshell, it lets you bind (unwrapped) parser results to names, which you can then use normally.
Let's just take a look at how it works:

    (defparser word []
      (many1 (letter)))

    (defparser greeting []
      (let->> [prefix (string "Hello, ")
               name (word)
               punctuation (choice (token= \.)
                                   (token= \!))]
        (if (= punctuation \!)
          (always [(apply str name) :excited])
          (always [(apply str name) :not-excited]))))

    (run (greeting) "Hello, Cat!")
    ; ["Cat" :excited]

    (run (greeting) "Hello, Dog.")
    ; ["Dog" :not-excited]

There's a lot happening here so let's look at it piece-by-piece.

First we use `defparser` to make a `word` function for creating word parsers.
We could have done this with `(def word (many1 (letter)))` and then used it as `word` later, but I find it's easier to just use `defparser` for everything.
That way we always get parsers the same way: by calling a function.

Next we have our `greeting` parser (technically a function that makes a parser, but you get the idea by now).
Inside we have a `let->>` that runs three parsers and binds their (unwrapped) results to names:

1. `(string "Hello, ")` parses a literal string.
   `prefix` gets bound to the string `"Hello, "`.
2. `(word)` parses one or more letters.
   `name` gets bound to the result, which is a sequence of chars like `(\C \a \t)`.
3. `(choice (token= \.) (token= \!))` parses a period or exclamation point.
   `punctuation` gets bound to the character that was parsed, like `\.` or `\!`.

That's it for the binding section.

Next we have the body of the `let->>`.
This needs to return a *wrapped* value, but we can do anything we like with our bound variables to determine what to return.
In this case we return different things depending on whether the greeting ended with an exclamation point or not.

Notice how the return values are wrapped in `(always ...)`.
Also notice how all the bound values have been unwrapped for us by `let->>`.
`name` really is just a sequence of characters which can be used with `(apply str ...)` as usual.

You might wonder whether you can move the `(apply str ...)` into the `let->>` binding form, so we don't have to do it twice.
Unfortunately you can't.
**Every right hand side in a `let->>` binding form has to evaluate to a parser**.

If you tried to do something like `(let->> [name (apply str (word))] ...)` it wouldn't work for two reasons.
First, `let->>` evaluates the right hand side and expects the result to be a parser, which it then runs.
So it would call `(apply str some-word-parser)` and get a string back, which isn't a parser.

Second, `let->>` unwraps the return value of `(word)` right before it binds it, so even if the first problem weren't true, `(apply str ...)` would get a wrapped value as its argument, which is not going to work.

Of course, you can do anything you want in the *body* of a `let->>`, so this is
fine:

    (let->> [name (word)]
      (let [name (apply str name)]
        (always name)))

`let` in this example is a vanilla Clojure `let`.

Binding forms in a `let->>` are executed in order, and importantly, later forms can refer to earlier ones.
Look at this example:

    (defparser sample []
      (let->> [sign (choice (token= \+)
                            (token= \-))
               word (if (= sign \+)
                      (string "plus")
                      (string "minus"))]
        (always [sign word])))

    (run (sample) "+plus")
    ; [\+ "plus"]

    (run (sample) "-minus")
    ; [\- "minus"]

    (run (sample) "+minus")
    ; ExceptionInfo Unexpected token 'm' at line: 1 column: 2  [...]

In this example, `sign` gets bound to the unwrapped result of the `choice` parser, which is a character.
Then we use that character to determine which parser to use in the next binding.
If the sign was a `\+`, we parse the string `"plus"`. Likewise for minus.

Notice how mixing the two in the last example produced an error.
We saw the `\+` and decided that we'd used the `(string "plus")` parser for the next input, but it turned out to be `"minus"`.

### Tips and Tricks

That's about it for the basics! You now know enough to parse a wide variety of things by building up complex parsers from very simple ones.

Before you go, here's a few tips and tricks that you might find helpful.

### You can parse more than just strings

Remember that the Parsatron operates on sequences of input.
These don't necessarily have to be strings.

Maybe you've got a big JSON response that you want to split apart.
Don't try to write a JSON parser from scratch, just use an existing one like [Cheshire](https://github.com/dakrone/cheshire) and then use the Parsatron to parse the Clojure datastructure(s) it sends back!

### You can throw away `let->>` bindings

Sometimes you're writing a `let->>` form and encounter a value that you don't really need to bind to a name.
Instead of stopping the `let->>` and nesting a `>>` inside it, just bind the value to a disposable name, like `_`:

    (defparser float []
      (let->> [integral (many1 (digit))
               _ (token= \.)
               fractional (many1 (digit))]
        (let [integral (apply str integral)
              fractional (apply str fractional)]
          (always (Double/parseDouble (str integral "." fractional))))))

    (run (float) "1.4")
    ; 1.4

    (run (float) "1.04")
    ; 1.04

    (run (float) "1.0400000")
    ; 1.04

## Roadmap

- **`0.1.0`**
  * baseline sane error messages
- **`0.4.0`**
  * website with documentation
    + at the least move github issue discussions into coherent pages
    + marginalia?
    + list of other good resources for learning parser combinators
- **`1.0`**
  * grammar support
    + put in a grammar in a BNF format and get back parsatron-implemented parsers
  * 5-7 non-trivial example parsers
    + at least one using a lexer + parsatron over a seq of lexical tokens, not chars
  * basic compile-time parser optimizations where possible
  * unified clj+cljs codebase + host-environment specific files
    + low-magic build process
    + one command to run both clj & cljs tests


## License

Copyright (C) 2011 Nate Young

Distributed under the Eclipse Public License, the same as Clojure.
