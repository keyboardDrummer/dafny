using Microsoft.Boogie;
using Microsoft.Dafny.LanguageServer.Util;
using Lsp = OmniSharp.Extensions.LanguageServer.Protocol;
using OmniSharp.Extensions.LanguageServer.Protocol.Models;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading;
using VCGeneration;

namespace Microsoft.Dafny.LanguageServer.Language {
  public class DiagnosticErrorReporter : ErrorReporter {
    private const MessageSource VerifierMessageSource = MessageSource.Verifier;
    private const string RelatedLocationCategory = "Related location";
    private const string RelatedLocationMessage = RelatedLocationCategory;

    private readonly Lsp.DocumentUri entryDocumentUri;
    private readonly Dictionary<Lsp.DocumentUri, List<Diagnostic>> diagnostics = new();
    private readonly Dictionary<DiagnosticSeverity, int> counts = new();
    private readonly Dictionary<DiagnosticSeverity, int> countsNotVerificationOrCompiler = new();
    private readonly ReaderWriterLockSlim rwLock = new();

    /// <summary>
    /// Creates a new instance with the given uri of the entry document.
    /// </summary>
    /// <param name="entryDocumentUri">The entry document's uri.</param>
    /// <remarks>
    /// The uri of the entry document is necessary to report general compiler errors as part of this document.
    /// </remarks>
    public DiagnosticErrorReporter(Lsp.DocumentUri entryDocumentUri) {
      this.entryDocumentUri = entryDocumentUri;
    }

    public IReadOnlyDictionary<Lsp.DocumentUri, List<Diagnostic>> AllDiagnostics => diagnostics;

    public IReadOnlyList<Diagnostic> GetDiagnostics(Lsp.DocumentUri documentUri) {
      rwLock.EnterReadLock();
      try {
        // For untitled documents, the URI needs to have a "untitled" scheme
        // to match what the client requires in the `diagnostics` dictionary.
        // We achieve this by expanding it into a file system path and parsing it again.
        var alternativeUntitled = documentUri.GetFileSystemPath();
        // Concurrency: Return a copy of the list not to expose a reference to an object that requires synchronization.
        // LATER: Make the Diagnostic type immutable, since we're not protecting it from concurrent accesses
        return new List<Diagnostic>(
          diagnostics.GetValueOrDefault(documentUri) ??
          diagnostics.GetValueOrDefault(alternativeUntitled) ??
          Enumerable.Empty<Diagnostic>());
      }
      finally {
        rwLock.ExitReadLock();
      }
    }

    public void ReportBoogieError(ErrorInformation error) {
      var relatedInformation = new List<DiagnosticRelatedInformation>();
      foreach (var auxiliaryInformation in error.Aux) {
        if (auxiliaryInformation.Category == RelatedLocationCategory) {
          relatedInformation.AddRange(CreateDiagnosticRelatedInformationFor(auxiliaryInformation.Tok, auxiliaryInformation.Msg));
        } else {
          // The execution trace is an additional auxiliary which identifies itself with
          // line=0 and character=0. These positions cause errors when exposing them, Furthermore,
          // the execution trace message appears to not have any interesting information.
          if (auxiliaryInformation.Tok.line > 0) {
            Info(VerifierMessageSource, auxiliaryInformation.Tok, auxiliaryInformation.Msg);
          }
        }
      }

      var uri = GetDocumentUriOrDefault(error.Tok);
      var diagnostic = new Diagnostic {
        Severity = DiagnosticSeverity.Error,
        Message = error.Msg,
        Range = error.Tok.GetLspRange(),
        RelatedInformation = relatedInformation,
        Source = VerifierMessageSource.ToString()
      };
      AddDiagnosticForFile(
        diagnostic,
        VerifierMessageSource,
        uri
      );
    }

    private static IEnumerable<DiagnosticRelatedInformation> CreateDiagnosticRelatedInformationFor(IToken token, string message) {
      yield return new DiagnosticRelatedInformation {
        Message = message,
        Location = CreateLocation(token)
      };
      if (token is NestedToken nestedToken) {
        foreach (var nestedInformation in CreateDiagnosticRelatedInformationFor(nestedToken.Inner, RelatedLocationMessage)) {
          yield return nestedInformation;
        }
      }
    }

    // public static Lsp.Position GetLspPosition(Position position) {
    //   return new Lsp.Range(
    //     range.,
    //     ToLspPosition(other.line, other.col + other.val.Length)
    //   );
    // }
    //
    // public static Lsp.Range GetLspRange(Range range) {
    //   return new Lsp.Range(
    //     range.,
    //     ToLspPosition(other.line, other.col + other.val.Length)
    //   );
    // }

    private static Location CreateLocation(FileRange fileRange) {
      return new Location {
        Range = fileRange.Range,

        // During parsing, we store absolute paths to make reconstructing the Uri easier
        // https://github.com/dafny-lang/dafny/blob/06b498ee73c74660c61042bb752207df13930376/Source/DafnyLanguageServer/Language/DafnyLangParser.cs#L59 
        Uri = token.GetDocumentUri()
      };
    }

    public override bool Message(MessageSource source, ErrorLevel level, FileRange fileRange, string msg) {
      if (ErrorsOnly && level != ErrorLevel.Error) {
        return false;
      }
      var relatedInformation = new List<DiagnosticRelatedInformation>();
      if (fileRange is NestedToken nestedToken) {
        relatedInformation.AddRange(
          CreateDiagnosticRelatedInformationFor(
            nestedToken.Inner, nestedToken.Message ?? "Related location")
        );
      }
      var item = new Diagnostic {
        Severity = ToSeverity(level),
        Message = msg,
        Range = fileRange.GetLspRange(),
        Source = source.ToString(),
        RelatedInformation = relatedInformation,
      };
      AddDiagnosticForFile(item, source, GetDocumentUriOrDefault(fileRange));
      return true;
    }

    public override int Count(ErrorLevel level) {
      rwLock.EnterReadLock();
      try {
        return counts.GetValueOrDefault(ToSeverity(level), 0);
      }
      finally {
        rwLock.ExitReadLock();
      }
    }

    public override int CountExceptVerifierAndCompiler(ErrorLevel level) {
      rwLock.EnterReadLock();
      try {
        return countsNotVerificationOrCompiler.GetValueOrDefault(ToSeverity(level), 0);
      }
      finally {
        rwLock.ExitReadLock();
      }
    }

    private void AddDiagnosticForFile(Diagnostic item, MessageSource messageSource, Lsp.DocumentUri documentUri) {
      rwLock.EnterWriteLock();
      try {
        var severity = item.Severity!.Value; // All our diagnostics have a severity.
        counts[severity] = counts.GetValueOrDefault(severity, 0) + 1;
        if (messageSource != MessageSource.Verifier && messageSource != MessageSource.Compiler) {
          countsNotVerificationOrCompiler[severity] =
            countsNotVerificationOrCompiler.GetValueOrDefault(severity, 0) + 1;
        }
        diagnostics.GetOrCreate(documentUri, () => new List<Diagnostic>()).Add(item);
      }
      finally {
        rwLock.ExitWriteLock();
      }
    }

    private Lsp.DocumentUri GetDocumentUriOrDefault(IToken token) {
      return token.filename == null
        ? entryDocumentUri
        : token.GetDocumentUri();
    }

    private static DiagnosticSeverity ToSeverity(ErrorLevel level) {
      return level switch {
        ErrorLevel.Error => DiagnosticSeverity.Error,
        ErrorLevel.Warning => DiagnosticSeverity.Warning,
        ErrorLevel.Info => DiagnosticSeverity.Hint,
        _ => throw new ArgumentException($"unknown error level {level}", nameof(level))
      };
    }
  }
}
