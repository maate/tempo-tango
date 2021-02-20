
tempo-tango is a .NET interpreter for Linear Temporal Logic in F#.

Check out the wiki for a more elaborate introduction: https://github.com/maate/tempo-tango/wiki

It is based on [this](https://github.com/tomykaira/ltl2ba) ML implementation by tomykaira

## Quick Start
In C#:

```csharp
    var tempo = new Tempo( "art ? ( adj W noun )" );

    tempo.Tango( "art", "noun" );  // true
    tempo.Tango( "noun", "noun" ); // false

    tempo.TangosWith( "art" )      // true
    tempo.TangosWith( "adj" )      // true
    tempo.TangosWith( "noun" )     // true
    tempo.TangosWith( "verb" )     // false
```

In F#:
```fsharp
    let tempo = new Tempo( "art ? ( adj W noun )" )

    tempo.Tango [ "art"; "noun" ]  // true
    tempo.Tango [ "noun"; "noun" ] // false
    
    tempo.TangosWith "art"         // true
    tempo.TangosWith "adj"         // true
    tempo.TangosWith "noun"        // true
    tempo.TangosWith "verb"        // false
```

Internally, the expression `art ? ( adj W noun )` is converted to this automaton:

![GBA](/docs/np-example.png)

The input symbols is then checked against the automaton. The symbols are evaluated as a run in the automaton.

# Supported Operators

Proposition names are written in alphanumeric lowercase.

## Quick overview of supported operators
The following operators are supported.

```
Operator   Alternatives   Meaning
T          true           True
E          ε              Empty string
!          ¬              Not
X                         Next
F                         Finally
G                         Globally
&          ∧              And
|          ∨              Or
U                         Until
W                         Weak Until
R                         Releases
?                         Binary Optional (either l holds, and then r holds next, or r holds)
```

Tempo-tango talks about time as events in the future. Tempo-tango extends normal propositional logic by including a time dimension. All supported operators are described below.

## Symbols from propositional logic
Tempo-tango supports the basic propositional symbols:

```
Operator   Alternatives   Meaning
T          true           True
E          ε              Empty string
!          ¬              Not
&          ∧              And
|          ∨              Or
->                        Implication
```

### True expression (T or true)
The true expression matches any input symbol.

For example:

```
let tempo = new Tempo( "T" )
tempo.Tango [ [ "art" ] ]     // success
```

### Empty expression (E or ε)
The empty expression matches only no-inputs and empty strings.

For example:

```
let tempo = new Tempo( "E" )
tempo.Tango [ [ "art" ] ]     // failure
tempo.Tango [ [ ] ]           // success
tempo.Tango [ [ "" ] ]        // success
```

Empty expressions can of course be negated in which case it matches an input symbol if the symbol is not an empty string.

For example:

```
let tempo = new Tempo( "!E" )
tempo.Tango [ [ "art" ] ]     // success
tempo.Tango [ [ ] ]           // failure
tempo.Tango [ [ "" ] ]        // failure
```

### Not operator (! or ¬)
The not operator negates the immediately following expression. It matches all input symbols which are not matched by the negated expression.

For example:

```
let tempo = new Tempo( "!a" )
tempo.Tango [ [ "a" ] ]       // failure
tempo.Tango [ [ "b" ] ]       // success
```

### And operator (& or ∧)
The and operator conjoins two expressions. It matches all inputs which are matched by both of the conjoined expressions.

For example:

```
let tempo = new Tempo( "a&b" )
tempo.Tango [ [ "a" ] ]           // failure (b was not matched)
tempo.Tango [ [ "b" ] ]           // failure (a was not matched)
tempo.Tango [ [ "a" ]; [ "b" ] ]  // failure (at first point in time, b was not matched)
tempo.Tango [ [ "a"; "b" ] ]      // success
```

### Or operator (| or ∨)
The and operator disjoins two expressions. It matches all inputs which are matched by one of the disjoined expressions.

For example:

```
let tempo = new Tempo( "a|b" )
tempo.Tango [ [ "a" ] ]           // success
tempo.Tango [ [ "b" ] ]           // success
tempo.Tango [ [ "a"; "b" ] ]      // success (matches both a and b)
```

### Implication operator (->)
The implication operator is a binary operator. It states that it cannot be the case that the left side matches without the right side matching as well. Internally, implication is syntactic sugar for the expression `¬(a & ¬b)` which is equivalent to and in fact internally converted to (but intuitively slightly harder to understand, I think) `¬a | b`.

For example:

```
let tempo = new Tempo( "a->b" )
tempo.Tango [ [ "b" ] ]           // success
tempo.Tango [ [ "a" ] ]           // failure (b is missing)
tempo.Tango [ [ "a"; "b" ] ]      // success
```

## Symbols from Linear Temporal Logic
Tempo-tango extends the normal propositional logic with a time dimension. In the API this is represented as a set of sets.

For example:

```
[
  [ "a" ];     // at the first point in time, a is the input
  [ "b" ];     // at the second point in time, b is the input
  [ "a"; "b"]  // at the third point in time, a and b are the input
]
```

The operators from linear temporal logic allows tempo-tango to talk about the time dimension.

```
Operator   Alternatives   Meaning
X                         Next
F                         Finally
G                         Globally
U                         Until
W                         Weak Until
R                         Releases
?                         Binary Optional (either l holds, and then r holds next, or r holds)
```

### Next operator (X)
The Next operator is a unary operator. It matches all inputs which are matched by the expression at the next point in time.

For example:

```
let tempo = new Tempo( "a&Xb" )
tempo.Tango [ [ "a" ] ]           // failure (a was matched, but b has to be matched at the next point in time)
tempo.Tango [ [ "b" ] ]           // failure (a had to be matched at the first point in time)
tempo.Tango [ [ "a" ]; [ "b" ] ]  // success (first matches a, then matches b)
tempo.Tango [ [ "a"; "b" ] ]      // failure (a and be both matches at the first point in time, but b has to be matched at the next point in time)
```

### Finally operator (F)
The Finally operator is a unary operator. It matches all inputs which are matched by the expression at some point in time.

For example:

```
let tempo = new Tempo( "Fb" )
tempo.Tango [ [ "a" ] ]           // failure (a was matched, but b has to be matched at some point in time)
tempo.Tango [ [ "b" ] ]           // success (b matches immediately)
tempo.Tango [ [ "a" ]; [ "b" ] ]  // success (first matches a, then matches b)
tempo.Tango [ [ "a"; "b" ] ]      // success (b matches immediately)
```

### Globally operator (G)
The Globally operator is a unary operator. It matches all inputs which are matched by the expression at any point in time.

For example:

```
let tempo = new Tempo( "Gb" )
tempo.Tango [ [ "a" ] ]           // failure (a was matched, but b has to be matched at all points in time)
tempo.Tango [ [ "b" ] ]           // success (b matches at all points in time)
tempo.Tango [ [ "a" ]; [ "b" ] ]  // failure (b was not matched at the first point in time)
tempo.Tango [ [ "a"; "b" ] ]      // success (b matches at all points in time)
```

### Until operator (U)
The Until operator is a binary operator. It matches all inputs which are matched by the left side of the expression until the right side of the expression is matched.

For example:

```
let tempo = new Tempo( "aUb" )
tempo.Tango [ [ "a" ] ]           // failure (a was matched, but b has to be matched)
tempo.Tango [ [ "b" ] ]           // success (b matches immediately)
tempo.Tango [ [ "a" ]; [ "b" ] ]  // success
tempo.Tango [ [ "a"; "b" ] ]      // success (b matches immediately; note that *only* b is matched here)
```

### WeakUntil operator (W)
The Until operator is a binary operator. It matches all inputs which are matched by the left side of the expression until the right side of the expression is optionally matched.

For example:

```
let tempo = new Tempo( "aWb" )
tempo.Tango [ [ "a" ] ]           // success (a was matched, and b does not have to be matched)
tempo.Tango [ [ "b" ] ]           // success (b matches immediately)
tempo.Tango [ [ "a" ]; [ "b" ] ]  // success
tempo.Tango [ [ "a"; "b" ] ]      // success (b matches immediately; note that *only* b is matched here)
```

### Optional operator (?)
The Optional operator is a binary operator. It matches either the left side of the operator and next the right side. Or it matches the right side at the current point in time.

For example:

```
let tempo = new Tempo( "a?b" )
tempo.Tango [ [ "a" ] ]           // failure (if a is matched, b has to be matched at the next point in time)
tempo.Tango [ [ "b" ] ]           // success
tempo.Tango [ [ "a" ]; [ "b" ] ]  // success
tempo.Tango [ [ "a"; "b" ] ]      // failure (if a is matched, b has to be matched at the next point in time)
```

# Run's
Tempo-tango is initialized by using the `Tempo` class:

In F#:

```fsharp
let tempo = new Tempo( "art ? ( adj W noun )" )
```

In C#:

```csharp
var tempo = new Tempo( "art ? ( adj W noun )" );
```

In the constructor you provide the linear temporal logic expression that you want the `Tempo` class to use. You can use the operators and expressions defined on this page.

As a convention, use lower case alphanumeric strings to present propositional symbols. Use upper case strings (and symbols) to present built-in operators and expressions.

In the above expression `art ? ( adj W noun )` the lower cased `art`, `adj`, and `noun` are my propositional symbols. These I can later use as input to the `Tempo` class. The rest are built-in constructs: `?` is the optional operator, `W` is the WeakUntil operator, and the parenthesis signal precedence.

After having initialized the `Tempo` class, you can now perform a run. Do this by calling the `Tango` method.

In F#:

```fsharp
tempo.Tango [ [ "art" ]; [ "noun" ] ]
```

In C#:

```csharp
_tempo.Tango( new[] {
  new[] { "art" },
  new[] { "noun" }
} );
```

This will input `art` at the first point in time, and `noun` at the second point in time.

If some point in time only has one input symbol, I call this a singleton input. For convenience, you can specify singleton inputs by using a `params` overload.

In C#:

```csharp
_tempo.Tango( "art", "noun" );
```

## Partial matches are accepted

Using the example above, in F#

```fsharp
let tempo = new Tempo( "art ? ( adj W noun )" )
tempo.Tango [ [ "art"; "sing" ]; [ "noun" ] ]
```

Here, the expression does not say anything about `sing` symbols. The run will not fail when it encounters an input symbol it doesn't understand, but in the return value, it will only return the `art` which it could match.

## Infinite matches
Internally, the linear temporal logic expression is converted to a specific type of automaton called a Büchi automaton. Büchi automata has the property that they run infinitely. This is convenient when dealing with time and having to talk about points in the future: *p at some point in time*, *p at all points in time*.

The infinite nature of the automaton can however give some unexpected results. This becomes more apparent if we take a closer look at the example expression `art ? ( adj W noun )`. This is converted to the following Büchi automaton.

![](https://github.com/maate/tempo-tango/blob/master/docs/np-example.png)

Superficially, it looks as we would expect. But note the endless loop on the node with ID 0000 in the bottom. This matches any symbol. As a consequence, the following expression will match (as intended):

* `[ [ art ]; [ adj ] ]`
* `[ [ art ]; [ adj ]; [ adj ] ]`
* `[ [ art ]; [ adj ]; [ adj ]; [ noun ] ]`
* `[ [ art ]; [ noun ] ]`

But what might be unexpected is that the following expressions will also match:

* `[ [ art ]; [ adj ]; [ adj ]; [ noun ]; [ verb ]; [ anothersentence ] ]`
* `[ [ art ]; [ noun ]; [ art ] ]`

This is due to the infinite nature of the automaton (specifically the endless loop on node `0000` in the automaton). In you expressions, you need to carefully work around this.

## License

The MIT License (MIT)

Copyright (c) 2016 Morten Maate

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
