// RUN: %baredafny resolve "%S/dafny.toml" > "%t"
// RUN: ! %baredafny resolve "%S/broken/dafny.toml" 2>> "%t"
// RUN: ! %baredafny resolve "%S/doesNotExist/dafny.toml" 2>> "%t"
// RUN: %diff "%s.expect" "%t"