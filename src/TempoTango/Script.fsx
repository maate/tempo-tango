// Learn more about F# at http://fsharp.net. See the 'F# Tutorial' project
// for more guidance on F# programming.

//#load "Library1.fs"
//open tempo_tango

#r "../packages/FParsec.1.0.2/lib/net40-client/FParsec.dll"
#r "../packages/FParsec.1.0.2/lib/net40-client/FParsecCS.dll"

#load "Parser.fs"
open TempoTango.Parser

Parser.Parse( "true" )

