
tempo-tango is a .NET interpreter for Linear Temporal Logic in F#.

It is based on [this](https://github.com/tomykaira/ltl2ba) ML implementation by tomykaira

## Quick Start
In C#:

```csharp
    var tempo = new Tempo( "art ? ( adj W noun )" );
    var result1 = tempo.Tango( "art", "noun" );  // true
    var result2 = tempo.Tango( "noun", "noun" ); // false
```

In F#:
```fsharp
    let tempo = new Tempo( "art ? ( adj W noun )" )
    let result1 = tempo.Tango [ "art"; "noun" ]  // true
    let result2 = tempo.Tango [ "noun"; "noun" ] // false
```

## Supported Operators

Proposition names are written in alphanumeric lowercase.

The following operators are supported.

```
Operator   Alternatives   Meaning
T          true           True
F          false,⊥        False
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
