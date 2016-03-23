namespace TempoTango.Console {
  internal class Program {
    
    private static void Main( string[] args ) {
      var tempo = new Tempo( "a & Xb" );
      var result = tempo.Tango( new[] { "a", "b" } );
      System.Console.WriteLine( result );
      System.Console.ReadKey();

    }

  }
}