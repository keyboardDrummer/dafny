// RUN: %dafny_0 /compile:0 /verifySnapshots:3 /traceCaching:1 %S/Inputs/Snapshots9.dfy > "%t"
// RUN: %diff "%s.expect" "%t"
