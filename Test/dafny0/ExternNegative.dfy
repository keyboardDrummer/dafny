// RUN: %dafny_0 /compile:0 /print:"%t.print" /dprint:"%t.dprint" "%s" > "%t"
// RUN: %diff "%s.expect" "%t"
module {:extern "Modx"} Mod1
{
  class {:extern "classx"} Class1
  {
    static function method {:extern "Fun1x"} Fun1() : int
      ensures Fun1() > 0
    static method {:extern "Method1x"} Method1() returns (x: int)
      ensures x > 0
    static abstract function method Fun2() : int
      ensures Fun2() > 0
    {
      Fun1()
    }
    static static method Method2() returns (x: int)
      ensures x > 0
    {
      x := Method1();
    }
  }
}
// Will give error about duplicate CompileName for module.
module {:extern "Modx"} Mod2
{
}
