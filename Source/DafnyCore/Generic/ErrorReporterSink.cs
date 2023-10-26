namespace Microsoft.Dafny;

public class ErrorReporterSink : ErrorReporter {
  public ErrorReporterSink(DafnyOptions options) : base(options) { }

  protected override bool MessageCore(MessageSource source, ErrorLevel level, string errorId, IToken tok, string msg) {
    return false;
  }

  public override void Error(MessageSource source, string errorId, IToken tok, string msg) {

  }

  public override int Count(ErrorLevel level) {
    return 0;
  }

  public override int CountExceptVerifierAndCompiler(ErrorLevel level) {
    return 0;
  }
}