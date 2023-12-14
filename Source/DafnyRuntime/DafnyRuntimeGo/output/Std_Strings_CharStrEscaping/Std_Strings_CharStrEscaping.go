// Package Std_Strings_CharStrEscaping
// Dafny module Std_Strings_CharStrEscaping compiled into Go

package Std_Strings_CharStrEscaping

import (
  _dafny "dafny"
  os "os"
  _System "System_"
  Std_Wrappers "Std_Wrappers"
  Std_Concurrent "Std_Concurrent"
  Std_FileIOInternalExterns "Std_FileIOInternalExterns"
  Std_BoundedInts "Std_BoundedInts"
  Std_Base64 "Std_Base64"
  Std_Relations "Std_Relations"
  Std_Math "Std_Math"
  Std_Collections_Seq "Std_Collections_Seq"
  Std_Collections_Array "Std_Collections_Array"
  Std_Collections_Imap "Std_Collections_Imap"
  Std_Functions "Std_Functions"
  Std_Collections_Iset "Std_Collections_Iset"
  Std_Collections_Map "Std_Collections_Map"
  Std_Collections_Set "Std_Collections_Set"
  Std_Collections "Std_Collections"
  Std_DynamicArray "Std_DynamicArray"
  Std_FileIO "Std_FileIO"
  Std_Arithmetic_GeneralInternals "Std_Arithmetic_GeneralInternals"
  Std_Arithmetic_MulInternalsNonlinear "Std_Arithmetic_MulInternalsNonlinear"
  Std_Arithmetic_MulInternals "Std_Arithmetic_MulInternals"
  Std_Arithmetic_Mul "Std_Arithmetic_Mul"
  Std_Arithmetic_ModInternalsNonlinear "Std_Arithmetic_ModInternalsNonlinear"
  Std_Arithmetic_DivInternalsNonlinear "Std_Arithmetic_DivInternalsNonlinear"
  Std_Arithmetic_ModInternals "Std_Arithmetic_ModInternals"
  Std_Arithmetic_DivInternals "Std_Arithmetic_DivInternals"
  Std_Arithmetic_DivMod "Std_Arithmetic_DivMod"
  Std_Arithmetic_Power "Std_Arithmetic_Power"
  Std_Arithmetic_Logarithm "Std_Arithmetic_Logarithm"
  Std_Arithmetic_Power2 "Std_Arithmetic_Power2"
  Std_Arithmetic "Std_Arithmetic"
  Std_Strings_HexConversion "Std_Strings_HexConversion"
  Std_Strings_DecimalConversion "Std_Strings_DecimalConversion"
)
var _ _dafny.Dummy__
var _ = os.Args
var _ _System.Dummy__
var _ Std_Wrappers.Dummy__
var _ Std_Concurrent.Dummy__
var _ Std_FileIOInternalExterns.Dummy__
var _ Std_BoundedInts.Dummy__
var _ Std_Base64.Dummy__
var _ Std_Relations.Dummy__
var _ Std_Math.Dummy__
var _ Std_Collections_Seq.Dummy__
var _ Std_Collections_Array.Dummy__
var _ Std_Collections_Imap.Dummy__
var _ Std_Functions.Dummy__
var _ Std_Collections_Iset.Dummy__
var _ Std_Collections_Map.Dummy__
var _ Std_Collections_Set.Dummy__
var _ Std_Collections.Dummy__
var _ Std_DynamicArray.Dummy__
var _ Std_FileIO.Dummy__
var _ Std_Arithmetic_GeneralInternals.Dummy__
var _ Std_Arithmetic_MulInternalsNonlinear.Dummy__
var _ Std_Arithmetic_MulInternals.Dummy__
var _ Std_Arithmetic_Mul.Dummy__
var _ Std_Arithmetic_ModInternalsNonlinear.Dummy__
var _ Std_Arithmetic_DivInternalsNonlinear.Dummy__
var _ Std_Arithmetic_ModInternals.Dummy__
var _ Std_Arithmetic_DivInternals.Dummy__
var _ Std_Arithmetic_DivMod.Dummy__
var _ Std_Arithmetic_Power.Dummy__
var _ Std_Arithmetic_Logarithm.Dummy__
var _ Std_Arithmetic_Power2.Dummy__
var _ Std_Arithmetic.Dummy__
var _ Std_Strings_HexConversion.Dummy__
var _ Std_Strings_DecimalConversion.Dummy__

type Dummy__ struct{}


// Definition of class Default__
type Default__ struct {
  dummy byte
}

func New_Default___() *Default__ {
  _this := Default__{}

  return &_this
}

type CompanionStruct_Default___ struct {
}
var Companion_Default___ = CompanionStruct_Default___ {
}

func (_this *Default__) Equals(other *Default__) bool {
  return _this == other
}

func (_this *Default__) EqualsGeneric(x interface{}) bool {
  other, ok := x.(*Default__)
  return ok && _this.Equals(other)
}

func (*Default__) String() string {
  return "Std_Strings_CharStrEscaping.Default__"
}
func (_this *Default__) ParentTraits_() []*_dafny.TraitID {
  return [](*_dafny.TraitID){};
}
var _ _dafny.TraitOffspring = &Default__{}

func (_static *CompanionStruct_Default___) Escape(str _dafny.Sequence, mustEscape _dafny.Set, escape _dafny.CodePoint) _dafny.Sequence {
  var _166___accumulator _dafny.Sequence = _dafny.SeqOf()
  _ = _166___accumulator
  goto TAIL_CALL_START
  TAIL_CALL_START:
  if (_dafny.Companion_Sequence_.Equal(str, _dafny.SeqOf())) {
    return _dafny.Companion_Sequence_.Concatenate(_166___accumulator, str)
  } else if ((mustEscape).Contains((str).Select(0).(_dafny.CodePoint))) {
    _166___accumulator = _dafny.Companion_Sequence_.Concatenate(_166___accumulator, _dafny.SeqOf(escape, (str).Select(0).(_dafny.CodePoint)))
    var _in56 _dafny.Sequence = (str).Drop(1)
    _ = _in56
    var _in57 _dafny.Set = mustEscape
    _ = _in57
    var _in58 _dafny.CodePoint = escape
    _ = _in58
    str = _in56
    mustEscape = _in57
    escape = _in58
    goto TAIL_CALL_START
  } else {
    _166___accumulator = _dafny.Companion_Sequence_.Concatenate(_166___accumulator, _dafny.SeqOf((str).Select(0).(_dafny.CodePoint)))
    var _in59 _dafny.Sequence = (str).Drop(1)
    _ = _in59
    var _in60 _dafny.Set = mustEscape
    _ = _in60
    var _in61 _dafny.CodePoint = escape
    _ = _in61
    str = _in59
    mustEscape = _in60
    escape = _in61
    goto TAIL_CALL_START
  }
}
func (_static *CompanionStruct_Default___) Unescape(str _dafny.Sequence, escape _dafny.CodePoint) Std_Wrappers.Option {
  if (_dafny.Companion_Sequence_.Equal(str, _dafny.SeqOf())) {
    return Std_Wrappers.Companion_Option_.Create_Some_(str)
  } else if (((str).Select(0).(_dafny.CodePoint)) == (escape)) {
    if ((_dafny.IntOfUint32((str).Cardinality())).Cmp(_dafny.One) > 0) {
      var _167_valueOrError0 Std_Wrappers.Option = Companion_Default___.Unescape((str).Drop(2), escape)
      _ = _167_valueOrError0
      if ((_167_valueOrError0).IsFailure()) {
        return (_167_valueOrError0).PropagateFailure()
      } else {
        var _168_tl _dafny.Sequence = (_167_valueOrError0).Extract().(_dafny.Sequence)
        _ = _168_tl
        return Std_Wrappers.Companion_Option_.Create_Some_(_dafny.Companion_Sequence_.Concatenate(_dafny.SeqOf((str).Select(1).(_dafny.CodePoint)), _168_tl))
      }
    } else {
      return Std_Wrappers.Companion_Option_.Create_None_()
    }
  } else {
    var _169_valueOrError1 Std_Wrappers.Option = Companion_Default___.Unescape((str).Drop(1), escape)
    _ = _169_valueOrError1
    if ((_169_valueOrError1).IsFailure()) {
      return (_169_valueOrError1).PropagateFailure()
    } else {
      var _170_tl _dafny.Sequence = (_169_valueOrError1).Extract().(_dafny.Sequence)
      _ = _170_tl
      return Std_Wrappers.Companion_Option_.Create_Some_(_dafny.Companion_Sequence_.Concatenate(_dafny.SeqOf((str).Select(0).(_dafny.CodePoint)), _170_tl))
    }
  }
}
// End of class Default__