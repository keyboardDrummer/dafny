// RUN: %baredafny build %args --relax-definite-assignment --enforce-determinism "%s" &> "%t" || true
// RUN: %OutputCheck --file-to-check "%t" "%s"
// CHECK: The option relax-definite-assignment can not be used in conjunction with enforce-determinism.

method Foo() {
}
