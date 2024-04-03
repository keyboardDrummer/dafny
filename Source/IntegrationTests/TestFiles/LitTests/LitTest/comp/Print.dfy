// NONUNIFORM: https://github.com/dafny-lang/dafny/issues/4108 and https://github.com/dafny-lang/dafny/issues/2582
// RUN: %verify --unicode-char false --relax-definite-assignment "%s" > "%t"
// RUN: %run --no-verify --unicode-char false --target cs "%s" >> "%t"
// RUN: %run --no-verify --unicode-char false --target js "%s" >> "%t"
// RUN: %run --no-verify --unicode-char false --target go "%s" >> "%t"
// RUN: %run --no-verify --unicode-char false --target java "%s" >> "%t"
// RUN: %diff "%s.expect" "%t"

// Python salts hashes so they are not deterministic.

method Main() {
  PrintString();
}

method PrintString() {
  print "Strings in collections:\n";
  print "  ", ["abc", "def"], "\n";
  print "  ", [["abc", "def"]], "\n";
  print "  ", {"abc", "def"}, "\n";
  print "  ", [['a', 'b', 'c'], ['d', 'e', 'f']], "\n";
  var a : seq<seq<char>> := [[]];
  print "  ", a, "\n";
  var b : seq<char>;
  print "  ", [b], "\n";
  print "  ", [seq(5, x => 'a')], "\n";
}
