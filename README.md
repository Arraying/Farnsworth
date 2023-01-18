# Farnsworth

Good news, everyone!
Farnsworth is a toy S-List language that (hopefully one day) can be used to solve problems productively.
This language probably shouldn't be taken too seriously.

## 🧰 Features

These sweet zombie Jesus features are subject to change:
* Purely functional, everything is a function, no mutability.
* Everything is curried!
* Lazy evaluation strategy.
* Pattern matching.
* Can run files as well as a budget REPL.

## ⁉️ But Why?

Actually, quite a few reasons:
* Primarily learning: to get more experience in programming language theory (and retain the experience I already have).
* Closely related: to get me to practice writing Haskell.
* I like minimalism that doesn't constrain you: I want to be able to do small programming exercises without much bloat.
* It's fun to write programming languages and make decisions you know you will both regret down the road and be judged for by other opinionated programmers.
* All the cool people seem to have their own toy language these days...

## 📦 Installation & Usage

1. Install GHCup, and install Haskell, Cabal and Stack.
2. Clone this repository.
3. Run `stack build` to compile the project.
4. Use `stack exec Farnsworth-exe` or `stack exec Farnsworth-exe file.farn` to open the REPL or run a file, respectively.

**Note:** Binary installation and/or distribution will be provided at a later date.

## 💼 A Tour

Usually, one would write "hello world" in order to get a bare program going. 
Unfortunately, Farnsworth has no IO.
So instead, this tour will cover some of the basics in the language.

### 🌸 Examples

Examples can be found in the [samples](samples) directory.
They are minimal though, so for a better reference refer to this document.

### 💢 Expressions and Structure

Everything in Farnsworth is an expression, which I find pretty neat.
Farnsworth can be run in its REPL or by providing a file.
Both should be just a single expression.

### ⌨️ Typing

Currently, Farnsworth is dynamically typed, so the programmer is relied upon to ensure type safety.
There are plans to change this in the future.

### 🔢 Primitives

There are three built-in primitives: numbers (positive literals only), booleans (true/false) and lists.

**Numbers:**

```lisp
1
```

**Booleans:**

```lisp
True
False
```

**Lists:**

```lisp
Nil
(cons 1 Nil)
(cons 1 (cons 2 Nil))
(list)
(list 1 2 3 4 5 6 7 8 9 10)
```

Lists can be constructed either through `cons` or `list`.
There is no differnce between the two: `list` gets desugared into `cons`.

### 1️⃣ Unary Operations

There is currently a single unary operation, `-`. 
This negates a number.
The parentheses mimicking a function call are required.

```lisp
(- 1)
(- (- 1))
```

**Note:** the space here is required, as `-1` is a valid identifier.

### 2️⃣ Binary Operations

There is also a single binary operation, `cons`. 
This has been seen in the examples above, but is again shown here:
```lisp
(cons 1 Nil)
```

The last element of the list must be `Nil`.
Due to dynamic typing, this will not be enforced, but the behaviour is undefined if this is not the case.

### ❔ Conditionals

There is only one conditional: `if`, which takes in a condition and two branches, one for if the condition is true, and another for if it is false.

```lisp
(if True 1 2)
(if False 3 4)
```

The condition must evaluate to a boolean, again, this is not enforced statically.

### 🎰 Functions

There are two types of functions in Farnsworth: named and anonymous functions. 
Named functions have the ability to recursively call themselves, something that anonymous functions cannot do. 
Both functions are closures.

**Named Functions:**

```lisp
(fn one () 1)
(fn add (l r) (+ l r))
(fn fact (n) 
  (if (<= n 0) 
    1 
    (* n (fact (- n 1))))) 
```

**Anonymous Functions:**

```lisp
(\ () 1)
(\ (l r) (+ l r))
```

In order to apply a function, it needs to be put in the first position of `()`, followed by arguments if applicable.
This is identical to arithmetic such as `(+ 1 2)`, so it should not be unsurprising that `+` is just another function.

**Function Application:**

```lisp
((fn add (l r) (+ l r)) 1 2)
((\ (l r) (+ l r)) 1 2)
((fn fact (n) 
  (if (<= n 0) 
    1 
    (* n (fact (- n 1))))) 
  5)
```

All functions can be curried. 
In fact, internally, both named functions and anonymous functions desugar to the same function type, which can only accept up to one argument.

Considering the `+` funtion, a function that adds 100 to any number can be constructed:

```lisp
(\ (v) (+ 100))
```

Assuming this is now bound to the identifier `add100`, it can then be used as follows, and evaluates to `119`.

```lisp
(add100 19)
```

### 📇 Pattern Matching

Pattern matching can be used to elegantly define how an expression will be evaluated.
All primitives can be matched against, and a special binding pattern can be used to extract the value.

Matching constitutes of an expression that should be matched, and cases this expression should be matched against.
These cases define patterns and are evaluated in left-to-right order, and the branch of the first case that matches will be evaluated.
In the scenario that cases are not exhaustive, a runtime error will occur.
Deep-matching (nested patterns) is supported.

The following example matches the variable `x` to different number constants:

```lisp
(match x
  (case 1 123)
  (case 2 456)
  (case 789 0))
```
So, if `x` is `1`, this will evaluate to `123`, if it is `2` to `456` and lastly if it is `789` to `0`.
Please note that this is an example of a non-exhaustive match, since if `x` is e.g. `5` or a different data type, none of the cases will match.

Similarly, cases for booleans and empty lists can be made.
These are `True` and `False`, as well as `Nil`.
Thus, their patterns are the same as the primitive value literals.

Lists can be matched with a special `cons` pattern.
In order for a list to match, both the head and tail have to match the pattern.
An example:

```lisp
(match x
  (case Nil 0)
  (case (cons 1 Nil) 123))
```

Here, the first case would only match `Nil` (or `(list)`), and the second only `(cons 1 Nil)` (or `(list 1)`).

Binds can be used to give extra power to matches.
They are denoted as identifiers, just like function parameters.
A bind will always match, but beyond that, the value of the match will be injected into the environment of the expression to evaluate by the same name.

So, for example:

```lisp
(match x
  (case y (+ x y)))
```

In this example, the `y` pattern will always match, but also be injected into the environment. Thus, this expression is equivalent to `(+ x x)`. Such patterns are not useful in the top-level, but are extremely powerful with deep matching. This can be seen for example when writing a `map` function:

```lisp
(fn map (f l)
  (match l 
    (case Nil Nil)
    (case (cons a b) 
      (cons (f a) (map f b))))))
```

### 😴 Laziness

Farnsworth is lazy in its call-by-name evaluation strategy.
Computations are deferred (placed into thunks) until the absolute last minute.
This has an implication on function calls and lists.

Function arguments will only be evaluated if they are needed. 
The following snippet will not produce a runtime error: the argument is never used and thereby never evaluated.

```lisp
((\ (x) 1) (head Nil))
```

Once the argument is used, this will as expected error:

```lisp
((\ (x) x) (head Nil))
```

Similarly for lists, the evaluation of both the head and tail of a `cons` call will be deferred.
Therefore, when running the following snippet:

```lisp
(list 1 2 3 4 5 6)
```

The output is `Thunk : Thunk`. The first thunk is the head, and the second thunk is the tail (a thunk of a cons containing more thunks). This allows for infinite lists to be created.

As aforementioned, computation is delayed until the last possible point. 
In order to compute at this point, strictness is applied by the interpreter, which will recursively un-thunk all values.
Strictness is applied where necessary in constructs and built-in functions.
Furthermore, strictness is applied before printing out a value in the REPL.
There is currently no way to manually apply strictness (and this is not needed).

Coming back to lists, `Thunk : Thunk` is not particularly useful output. In order to recursively apply strictness to a list, the `force` function can be called. Applying `force` on an infinite list will not halt. An example can be seen below:

```lisp
(force (list 1 2 3 4 5 6))
```

## 📚 Standard Library

The standard libary is so minimal it should probably be called a standard bookshelf.
I plan on expanding it, eventually, but until then there's really just the basics.
Since the language is quite concise and powerful, having to write your own functions shouldn't be too bad.
As a reminder, all functions can be curried.

**Arity 0 Functions:**
| Name | Return Type | Description |
| --- | --- | --- |
| `best-number` | number | Returns `19`. |

**Arity 1 Functions:**

| Name | Type | Return Type | Description | 
| --- | --- | --- | --- |
| `id` | any | any | Returns the argument. |
| `!` | boolean | boolean | Logical NOT. |
| `force` | list | list | Forces a list (see above). |
| `head` | nonempty list | any | Returns the head of the list. |
| `tail` | nonempty list | any | Returns the tail of the list. |
| `nil?` | list | boolean | Returns true iff the list is empty. |
| `list?` | any | boolean | Returns true iff the argument is a list. |

**Arity 2 Functions:**

| Name | Left Type | Right Type | Return Type | Description | 
| --- | --- | --- | --- | --- |
| `+` | number | number | number | Addition. | 
| `-` | number | number | number | Subtraction. | 
| `*` | number | number | number | Multiplication. | 
| `/` | number | number | number | Integer (floor) division. | 
| `&&` | boolean | boolean | boolean | Logical AND. |
| `\|\|` | boolean | boolean | boolean | Logical OR. |
| `==` | boolean/number | boolean/number | boolean | Whether the two booleans/numbers are equal. |
| `!=` | boolean/number | boolean/number | boolean | Whether the two booleans/numbers are not equal. |
| `<` | number | number | boolean | Whether LHS < RHS. |
| `>` | number | number | boolean | Whether LHS > RHS. |
| `<=` | number | number | boolean | Whether LHS <= RHS. |
| `>=` | number | number | boolean | Whether LHS >= RHS. |
| `cons_` | any | nil or list | list | Curried version of `cons`. |


## 😊 Conventions

Farnsworth files should be in a `.farn` file that has an UpperCamelCase name.
Function names and other identifiers (parameters, case identifiers, etc.) should be lowerCamelCase.
Functions that return a boolean should be suffixed with `?`. Functions that perform side-effects (in the future!) should be suffixed with `!`.

Indentation and whitespace is up to the user.
It should be mentioned that not all whitespace can be ignored, since for example `(:123)` is not equivalent to `(: 123)`.

## ⚖️ License

Copyright Paul Hübner (c) 2023

All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

 * Redistributions of source code must retain the above copyright
  notice, this list of conditions and the following disclaimer.

  * Redistributions in binary form must reproduce the above
    copyright notice, this list of conditions and the following
    disclaimer in the documentation and/or other materials provided
    with the distribution.

  * Neither the name of Author name here nor the names of other
    contributors may be used to endorse or promote products derived
    from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.