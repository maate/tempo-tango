using M8.TempoTango;

namespace TempoTango.Console {
  internal class Program {
    
    private static void Main( string[] args ) {
      var tempo = new Tempo( "a & Xb" );
      var result = tempo.Tango( "a", "b" );
      System.Console.WriteLine( result );
      System.Console.ReadKey();

    }

  }
}