// RUN: %dafny /verifyAllModules /allocated:1 /compile:0 /dprint:"%t.dprint" /autoTriggers:1 /printTooltips "%s" > "%t"
// RUN: %diff "%s.expect" "%t"
include "../../dafny0/TriggerInPredicate.dfy"
