// Class __default
// Dafny class __default compiled into Java
package Std.JSON.ZeroCopy.Serializer;

import JavaInternal.*;
import Std.Wrappers.*;
import Std.FileIOInternalExterns.*;
import Std.BoundedInts.*;
import Std.Base64.*;
import Std.Math.*;
import Std.Collections.Seq.*;
import Std.Collections.Array.*;
import Std.Collections.Imap.*;
import Std.Collections.Map.*;
import Std.Collections.Set.*;
import Std.DynamicArray.*;
import Std.FileIO.*;
import Std.Arithmetic.MulInternals.*;
import Std.Arithmetic.ModInternals.*;
import Std.Arithmetic.DivInternals.*;
import Std.Arithmetic.DivMod.*;
import Std.Arithmetic.Power.*;
import Std.Arithmetic.Logarithm.*;
import Std.Arithmetic.Power2.*;
import Std.Strings.HexConversion.*;
import Std.Strings.DecimalConversion.*;
import Std.Strings.CharStrEscaping.*;
import Std.Strings.*;
import Std.Unicode.Base.*;
import Std.Unicode.Utf8EncodingForm.*;
import Std.Unicode.Utf16EncodingForm.*;
import Std.Unicode.UnicodeStringsWithUnicodeChar.*;
import Std.Unicode.Utf8EncodingScheme.*;
import Std.JSON.Values.*;
import Std.JSON.Errors.*;
import Std.JSON.Spec.*;
import Std.JSON.Utils.Views.Core.*;
import Std.JSON.Utils.Views.Writers.*;
import Std.JSON.Utils.Lexers.Core.*;
import Std.JSON.Utils.Lexers.Strings.*;
import Std.JSON.Utils.Cursors.*;
import Std.JSON.Utils.Parsers.*;
import Std.JSON.Grammar.*;
import Std.JSON.ByteStrConversion.*;
import Std.JSON.Serializer.*;
import Std.JSON.Deserializer.Uint16StrConversion.*;
import Std.JSON.Deserializer.*;
import Std.JSON.ConcreteSyntax.Spec.*;

@SuppressWarnings({"unchecked", "deprecation"})
public class __default {
  public __default() {
  }
  public static Std.Wrappers.Result<byte[], Std.JSON.Errors.SerializationError> Serialize(Std.JSON.Grammar.Structural<Std.JSON.Grammar.Value> js)
  {
    Std.Wrappers.Result<byte[], Std.JSON.Errors.SerializationError> rbs = Std.Wrappers.Result.<byte[], Std.JSON.Errors.SerializationError>Default(dafny.TypeDescriptor.BYTE_ARRAY, Std.JSON.Errors.SerializationError._typeDescriptor(), new byte[0]);
    Std.JSON.Utils.Views.Writers.Writer__ _579_writer;
    _579_writer = __default.Text(js);
    Std.Wrappers.OutcomeResult<Std.JSON.Errors.SerializationError> _580_valueOrError0 = Std.Wrappers.OutcomeResult.<Std.JSON.Errors.SerializationError>Default(Std.JSON.Errors.SerializationError._typeDescriptor());
    _580_valueOrError0 = Std.Wrappers.__default.<Std.JSON.Errors.SerializationError>Need(Std.JSON.Errors.SerializationError._typeDescriptor(), (_579_writer).Unsaturated_q(), Std.JSON.Errors.SerializationError.create_OutOfMemory());
    if ((_580_valueOrError0).IsFailure(Std.JSON.Errors.SerializationError._typeDescriptor())) {
      rbs = (_580_valueOrError0).<byte[]>PropagateFailure(Std.JSON.Errors.SerializationError._typeDescriptor(), dafny.TypeDescriptor.BYTE_ARRAY);
      return rbs;
    }
    byte[] _581_bs;
    byte[] _out11;
    _out11 = (_579_writer).ToArray();
    _581_bs = _out11;
    rbs = Std.Wrappers.Result.<byte[], Std.JSON.Errors.SerializationError>create_Success(dafny.TypeDescriptor.BYTE_ARRAY, Std.JSON.Errors.SerializationError._typeDescriptor(), _581_bs);
    return rbs;
  }
  public static Std.Wrappers.Result<java.lang.Integer, Std.JSON.Errors.SerializationError> SerializeTo(Std.JSON.Grammar.Structural<Std.JSON.Grammar.Value> js, byte[] dest)
  {
    Std.Wrappers.Result<java.lang.Integer, Std.JSON.Errors.SerializationError> len = Std.Wrappers.Result.<java.lang.Integer, Std.JSON.Errors.SerializationError>Default(Std.BoundedInts.uint32._typeDescriptor(), Std.JSON.Errors.SerializationError._typeDescriptor(), 0);
    Std.JSON.Utils.Views.Writers.Writer__ _582_writer;
    _582_writer = __default.Text(js);
    Std.Wrappers.OutcomeResult<Std.JSON.Errors.SerializationError> _583_valueOrError0 = Std.Wrappers.OutcomeResult.<Std.JSON.Errors.SerializationError>Default(Std.JSON.Errors.SerializationError._typeDescriptor());
    _583_valueOrError0 = Std.Wrappers.__default.<Std.JSON.Errors.SerializationError>Need(Std.JSON.Errors.SerializationError._typeDescriptor(), (_582_writer).Unsaturated_q(), Std.JSON.Errors.SerializationError.create_OutOfMemory());
    if ((_583_valueOrError0).IsFailure(Std.JSON.Errors.SerializationError._typeDescriptor())) {
      len = (_583_valueOrError0).<java.lang.Integer>PropagateFailure(Std.JSON.Errors.SerializationError._typeDescriptor(), Std.BoundedInts.uint32._typeDescriptor());
      return len;
    }
    Std.Wrappers.OutcomeResult<Std.JSON.Errors.SerializationError> _584_valueOrError1 = Std.Wrappers.OutcomeResult.<Std.JSON.Errors.SerializationError>Default(Std.JSON.Errors.SerializationError._typeDescriptor());
    _584_valueOrError1 = Std.Wrappers.__default.<Std.JSON.Errors.SerializationError>Need(Std.JSON.Errors.SerializationError._typeDescriptor(), (dafny.Helpers.unsignedToBigInteger((_582_writer).dtor_length())).compareTo(java.math.BigInteger.valueOf(java.lang.reflect.Array.getLength((dest)))) <= 0, Std.JSON.Errors.SerializationError.create_OutOfMemory());
    if ((_584_valueOrError1).IsFailure(Std.JSON.Errors.SerializationError._typeDescriptor())) {
      len = (_584_valueOrError1).<java.lang.Integer>PropagateFailure(Std.JSON.Errors.SerializationError._typeDescriptor(), Std.BoundedInts.uint32._typeDescriptor());
      return len;
    }
    (_582_writer).CopyTo(dest);
    len = Std.Wrappers.Result.<java.lang.Integer, Std.JSON.Errors.SerializationError>create_Success(Std.BoundedInts.uint32._typeDescriptor(), Std.JSON.Errors.SerializationError._typeDescriptor(), (_582_writer).dtor_length());
    return len;
  }
  public static Std.JSON.Utils.Views.Writers.Writer__ Text(Std.JSON.Grammar.Structural<Std.JSON.Grammar.Value> js) {
    return __default.JSON(js, Std.JSON.Utils.Views.Writers.Writer__.Empty());
  }
  public static Std.JSON.Utils.Views.Writers.Writer__ JSON(Std.JSON.Grammar.Structural<Std.JSON.Grammar.Value> js, Std.JSON.Utils.Views.Writers.Writer__ writer)
  {
    return (((writer).Append((js).dtor_before())).Then(((java.util.function.Function<Std.JSON.Grammar.Structural<Std.JSON.Grammar.Value>, java.util.function.Function<Std.JSON.Utils.Views.Writers.Writer__, Std.JSON.Utils.Views.Writers.Writer__>>)(_585_js) -> ((java.util.function.Function<Std.JSON.Utils.Views.Writers.Writer__, Std.JSON.Utils.Views.Writers.Writer__>)(_586_wr_boxed0) -> {
      Std.JSON.Utils.Views.Writers.Writer__ _586_wr = ((Std.JSON.Utils.Views.Writers.Writer__)(java.lang.Object)(_586_wr_boxed0));
      return __default.Value((_585_js).dtor_t(), _586_wr);
    })).apply(js))).Append((js).dtor_after());
  }
  public static Std.JSON.Utils.Views.Writers.Writer__ Value(Std.JSON.Grammar.Value v, Std.JSON.Utils.Views.Writers.Writer__ writer)
  {
    Std.JSON.Grammar.Value _source23 = v;
    if (_source23.is_Null()) {
      Std.JSON.Utils.Views.Core.View__ _587___mcc_h0 = ((Std.JSON.Grammar.Value_Null)_source23)._n;
      Std.JSON.Utils.Views.Core.View__ _588_n = _587___mcc_h0;
      Std.JSON.Utils.Views.Writers.Writer__ _589_wr = (writer).Append(_588_n);
      return _589_wr;
    } else if (_source23.is_Bool()) {
      Std.JSON.Utils.Views.Core.View__ _590___mcc_h1 = ((Std.JSON.Grammar.Value_Bool)_source23)._b;
      Std.JSON.Utils.Views.Core.View__ _591_b = _590___mcc_h1;
      Std.JSON.Utils.Views.Writers.Writer__ _592_wr = (writer).Append(_591_b);
      return _592_wr;
    } else if (_source23.is_String()) {
      Std.JSON.Grammar.jstring _593___mcc_h2 = ((Std.JSON.Grammar.Value_String)_source23)._str;
      Std.JSON.Grammar.jstring _594_str = _593___mcc_h2;
      Std.JSON.Utils.Views.Writers.Writer__ _595_wr = __default.String(_594_str, writer);
      return _595_wr;
    } else if (_source23.is_Number()) {
      Std.JSON.Grammar.jnumber _596___mcc_h3 = ((Std.JSON.Grammar.Value_Number)_source23)._num;
      Std.JSON.Grammar.jnumber _597_num = _596___mcc_h3;
      Std.JSON.Utils.Views.Writers.Writer__ _598_wr = __default.Number(_597_num, writer);
      return _598_wr;
    } else if (_source23.is_Object()) {
      Std.JSON.Grammar.Bracketed<Std.JSON.Utils.Views.Core.View__, Std.JSON.Grammar.jKeyValue, Std.JSON.Utils.Views.Core.View__, Std.JSON.Utils.Views.Core.View__> _599___mcc_h4 = ((Std.JSON.Grammar.Value_Object)_source23)._obj;
      Std.JSON.Grammar.Bracketed<Std.JSON.Utils.Views.Core.View__, Std.JSON.Grammar.jKeyValue, Std.JSON.Utils.Views.Core.View__, Std.JSON.Utils.Views.Core.View__> _600_obj = _599___mcc_h4;
      Std.JSON.Utils.Views.Writers.Writer__ _601_wr = __default.Object(_600_obj, writer);
      return _601_wr;
    } else {
      Std.JSON.Grammar.Bracketed<Std.JSON.Utils.Views.Core.View__, Std.JSON.Grammar.Value, Std.JSON.Utils.Views.Core.View__, Std.JSON.Utils.Views.Core.View__> _602___mcc_h5 = ((Std.JSON.Grammar.Value_Array)_source23)._arr;
      Std.JSON.Grammar.Bracketed<Std.JSON.Utils.Views.Core.View__, Std.JSON.Grammar.Value, Std.JSON.Utils.Views.Core.View__, Std.JSON.Utils.Views.Core.View__> _603_arr = _602___mcc_h5;
      Std.JSON.Utils.Views.Writers.Writer__ _604_wr = __default.Array(_603_arr, writer);
      return _604_wr;
    }
  }
  public static Std.JSON.Utils.Views.Writers.Writer__ String(Std.JSON.Grammar.jstring str, Std.JSON.Utils.Views.Writers.Writer__ writer)
  {
    return (((writer).Append((str).dtor_lq())).Append((str).dtor_contents())).Append((str).dtor_rq());
  }
  public static Std.JSON.Utils.Views.Writers.Writer__ Number(Std.JSON.Grammar.jnumber num, Std.JSON.Utils.Views.Writers.Writer__ writer)
  {
    Std.JSON.Utils.Views.Writers.Writer__ _605_wr1 = ((writer).Append((num).dtor_minus())).Append((num).dtor_num());
    Std.JSON.Utils.Views.Writers.Writer__ _606_wr2 = ((((num).dtor_frac()).is_NonEmpty()) ? (((_605_wr1).Append((((num).dtor_frac()).dtor_t()).dtor_period())).Append((((num).dtor_frac()).dtor_t()).dtor_num())) : (_605_wr1));
    Std.JSON.Utils.Views.Writers.Writer__ _607_wr3 = ((((num).dtor_exp()).is_NonEmpty()) ? ((((_606_wr2).Append((((num).dtor_exp()).dtor_t()).dtor_e())).Append((((num).dtor_exp()).dtor_t()).dtor_sign())).Append((((num).dtor_exp()).dtor_t()).dtor_num())) : (_606_wr2));
    Std.JSON.Utils.Views.Writers.Writer__ _608_wr = _607_wr3;
    return _608_wr;
  }
  public static Std.JSON.Utils.Views.Writers.Writer__ StructuralView(Std.JSON.Grammar.Structural<Std.JSON.Utils.Views.Core.View__> st, Std.JSON.Utils.Views.Writers.Writer__ writer)
  {
    return (((writer).Append((st).dtor_before())).Append((st).dtor_t())).Append((st).dtor_after());
  }
  public static Std.JSON.Utils.Views.Writers.Writer__ Object(Std.JSON.Grammar.Bracketed<Std.JSON.Utils.Views.Core.View__, Std.JSON.Grammar.jKeyValue, Std.JSON.Utils.Views.Core.View__, Std.JSON.Utils.Views.Core.View__> obj, Std.JSON.Utils.Views.Writers.Writer__ writer)
  {
    Std.JSON.Utils.Views.Writers.Writer__ _609_wr = __default.StructuralView((obj).dtor_l(), writer);
    Std.JSON.Utils.Views.Writers.Writer__ _610_wr = __default.Members(obj, _609_wr);
    Std.JSON.Utils.Views.Writers.Writer__ _611_wr = __default.StructuralView((obj).dtor_r(), _610_wr);
    return _611_wr;
  }
  public static Std.JSON.Utils.Views.Writers.Writer__ Array(Std.JSON.Grammar.Bracketed<Std.JSON.Utils.Views.Core.View__, Std.JSON.Grammar.Value, Std.JSON.Utils.Views.Core.View__, Std.JSON.Utils.Views.Core.View__> arr, Std.JSON.Utils.Views.Writers.Writer__ writer)
  {
    Std.JSON.Utils.Views.Writers.Writer__ _612_wr = __default.StructuralView((arr).dtor_l(), writer);
    Std.JSON.Utils.Views.Writers.Writer__ _613_wr = __default.Items(arr, _612_wr);
    Std.JSON.Utils.Views.Writers.Writer__ _614_wr = __default.StructuralView((arr).dtor_r(), _613_wr);
    return _614_wr;
  }
  public static Std.JSON.Utils.Views.Writers.Writer__ Members(Std.JSON.Grammar.Bracketed<Std.JSON.Utils.Views.Core.View__, Std.JSON.Grammar.jKeyValue, Std.JSON.Utils.Views.Core.View__, Std.JSON.Utils.Views.Core.View__> obj, Std.JSON.Utils.Views.Writers.Writer__ writer)
  {
    Std.JSON.Utils.Views.Writers.Writer__ wr = Std.JSON.Utils.Views.Writers.Writer.defaultValue();
    if(true) {
      Std.JSON.Utils.Views.Writers.Writer__ _out12;
      _out12 = __default.MembersImpl(obj, writer);
      wr = _out12;
    }
    return wr;
  }
  public static Std.JSON.Utils.Views.Writers.Writer__ Items(Std.JSON.Grammar.Bracketed<Std.JSON.Utils.Views.Core.View__, Std.JSON.Grammar.Value, Std.JSON.Utils.Views.Core.View__, Std.JSON.Utils.Views.Core.View__> arr, Std.JSON.Utils.Views.Writers.Writer__ writer)
  {
    Std.JSON.Utils.Views.Writers.Writer__ wr = Std.JSON.Utils.Views.Writers.Writer.defaultValue();
    if(true) {
      Std.JSON.Utils.Views.Writers.Writer__ _out13;
      _out13 = __default.ItemsImpl(arr, writer);
      wr = _out13;
    }
    return wr;
  }
  public static Std.JSON.Utils.Views.Writers.Writer__ MembersImpl(Std.JSON.Grammar.Bracketed<Std.JSON.Utils.Views.Core.View__, Std.JSON.Grammar.jKeyValue, Std.JSON.Utils.Views.Core.View__, Std.JSON.Utils.Views.Core.View__> obj, Std.JSON.Utils.Views.Writers.Writer__ writer)
  {
    Std.JSON.Utils.Views.Writers.Writer__ wr = Std.JSON.Utils.Views.Writers.Writer.defaultValue();
    if(true) {
      wr = writer;
      dafny.DafnySequence<? extends Std.JSON.Grammar.Suffixed<Std.JSON.Grammar.jKeyValue, Std.JSON.Utils.Views.Core.View__>> _615_members;
      _615_members = (obj).dtor_data();
      java.math.BigInteger _hi1 = java.math.BigInteger.valueOf((_615_members).length());
      for (java.math.BigInteger _616_i = java.math.BigInteger.ZERO; _616_i.compareTo(_hi1) < 0; _616_i = _616_i.add(java.math.BigInteger.ONE)) {
        wr = __default.Member(((Std.JSON.Grammar.Suffixed<Std.JSON.Grammar.jKeyValue, Std.JSON.Utils.Views.Core.View__>)(java.lang.Object)((_615_members).select(dafny.Helpers.toInt((_616_i))))), wr);
      }
    }
    return wr;
  }
  public static Std.JSON.Utils.Views.Writers.Writer__ ItemsImpl(Std.JSON.Grammar.Bracketed<Std.JSON.Utils.Views.Core.View__, Std.JSON.Grammar.Value, Std.JSON.Utils.Views.Core.View__, Std.JSON.Utils.Views.Core.View__> arr, Std.JSON.Utils.Views.Writers.Writer__ writer)
  {
    Std.JSON.Utils.Views.Writers.Writer__ wr = Std.JSON.Utils.Views.Writers.Writer.defaultValue();
    if(true) {
      wr = writer;
      dafny.DafnySequence<? extends Std.JSON.Grammar.Suffixed<Std.JSON.Grammar.Value, Std.JSON.Utils.Views.Core.View__>> _617_items;
      _617_items = (arr).dtor_data();
      java.math.BigInteger _hi2 = java.math.BigInteger.valueOf((_617_items).length());
      for (java.math.BigInteger _618_i = java.math.BigInteger.ZERO; _618_i.compareTo(_hi2) < 0; _618_i = _618_i.add(java.math.BigInteger.ONE)) {
        wr = __default.Item(((Std.JSON.Grammar.Suffixed<Std.JSON.Grammar.Value, Std.JSON.Utils.Views.Core.View__>)(java.lang.Object)((_617_items).select(dafny.Helpers.toInt((_618_i))))), wr);
      }
    }
    return wr;
  }
  public static Std.JSON.Utils.Views.Writers.Writer__ Member(Std.JSON.Grammar.Suffixed<Std.JSON.Grammar.jKeyValue, Std.JSON.Utils.Views.Core.View__> m, Std.JSON.Utils.Views.Writers.Writer__ writer)
  {
    Std.JSON.Utils.Views.Writers.Writer__ _619_wr = __default.String(((m).dtor_t()).dtor_k(), writer);
    Std.JSON.Utils.Views.Writers.Writer__ _620_wr = __default.StructuralView(((m).dtor_t()).dtor_colon(), _619_wr);
    Std.JSON.Utils.Views.Writers.Writer__ _621_wr = __default.Value(((m).dtor_t()).dtor_v(), _620_wr);
    Std.JSON.Utils.Views.Writers.Writer__ _622_wr = ((((m).dtor_suffix()).is_Empty()) ? (_621_wr) : (__default.StructuralView(((m).dtor_suffix()).dtor_t(), _621_wr)));
    return _622_wr;
  }
  public static Std.JSON.Utils.Views.Writers.Writer__ Item(Std.JSON.Grammar.Suffixed<Std.JSON.Grammar.Value, Std.JSON.Utils.Views.Core.View__> m, Std.JSON.Utils.Views.Writers.Writer__ writer)
  {
    Std.JSON.Utils.Views.Writers.Writer__ _623_wr = __default.Value((m).dtor_t(), writer);
    Std.JSON.Utils.Views.Writers.Writer__ _624_wr = ((((m).dtor_suffix()).is_Empty()) ? (_623_wr) : (__default.StructuralView(((m).dtor_suffix()).dtor_t(), _623_wr)));
    return _624_wr;
  }
  @Override
  public java.lang.String toString() {
    return "Std.JSON.ZeroCopy.Serializer._default";
  }
}
