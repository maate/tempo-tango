using M8.TempoTango;

namespace TempoTango.Console {
  internal class Program {
    private static void Main( string[] args ) {
      var tempo = new Tempo( "a & Xb" );

      //SimpleTangoTests.Run();
      //return;
      MatchInfo result = tempo.Tango( "a", "b" );
      System.Console.WriteLine( result );
      //foreach ( var item in result.Matches ) {
      //  foreach ( string i in item ) {
      //    System.Console.WriteLine( i );
      //  }
      //}
      System.Console.ReadKey();
    }
  }
}