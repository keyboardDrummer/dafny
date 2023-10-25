namespace Microsoft.Dafny.LanguageServer.Workspace;

public enum PublishedVerificationStatus {
  Stale = 0,    // Not scheduled to be run
  Queued = 1,   // Scheduled to be run but waiting for resources
  Running = 2,  // Currently running
  Error = 4,    // Finished and had errors
  Correct = 5,  // Finished and was correct
}