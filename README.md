
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

## Supported Operators

Proposition names are written in alphanumeric lowercase.

The following operators are supported.

```
Operator   Alternatives   Meaning
T          true           True
F          false,⊥        False
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

## License

The MIT License (MIT)

Copyright (c) 2016 Morten Maate

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
