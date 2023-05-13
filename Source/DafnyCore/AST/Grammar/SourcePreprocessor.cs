﻿using System;
using System.Collections.Generic;
using System.Diagnostics.Contracts;
using System.IO;
using System.Text;

namespace Microsoft.Dafny;

// Dafny files can use preprocessing directives, e.g.
/// ```
/// method TEst() {
/// #if UNDEFINEDVARIABLE
///   assert false;
/// #else
///   assert true;
/// #endif
/// }
/// ```
/// is transformed into
/// ```
/// method TEst() {
/// 
/// 
/// 
///   assert true;
/// 
/// }
/// ```
/// 
/// However, at this moment, there is no way to tell Dafny which preprocessing variables
/// are defined, so only all other blocks than "else" are replaced by empty newlines
/// Note that, because this process replaces newlines, this version of SourcePreprocessor
/// recover existing newlines to ensure that, if there is no pre-processing directives,
/// the program string is exactly the same as the original one.
public static class SourcePreprocessor {
  struct IfDirectiveState {
    public bool hasSeenElse;
    public bool mayStillIncludeAnotherAlternative;

    public IfDirectiveState(bool hasSeenElse, bool mayStillIncludeAnotherAlternative) {
      this.hasSeenElse = hasSeenElse;
      this.mayStillIncludeAnotherAlternative = mayStillIncludeAnotherAlternative;
    }
  }

  // "arg" is assumed to be trimmed
  private static bool IfdefConditionSaysToInclude(string arg, List<string> /*!*/ defines) {
    Contract.Requires(arg != null);
    Contract.Requires(cce.NonNullElements(defines));
    bool sense = true;
    while (arg.StartsWith("!")) {
      sense = !sense;
      arg = arg.Substring(1).TrimStart();
    }

    return defines.Contains(arg) == sense;
  }

  public static string ProcessDirectives(TextReader reader, List<string> /*!*/ defines) {
    Contract.Requires(reader != null);
    Contract.Requires(cce.NonNullElements(defines));
    Contract.Ensures(Contract.Result<string>() != null);
    string newline = null;
    StringBuilder sb = new StringBuilder();
    List<IfDirectiveState> /*!*/
      ifDirectiveStates = new List<IfDirectiveState>(); // readState.Count is the current nesting level of #if's
    int ignoreCutoff =
      -1; // -1 means we're not ignoring; for 0<=n, n means we're ignoring because of something at nesting level n
    while (true)
    //invariant -1 <= ignoreCutoff && ignoreCutoff < readState.Count;
    {
      string line;
      if (newline == null) {
        line = ReadLineAndDetermineNewline(reader, out newline);
      } else {
        line = reader.ReadLine();
      }
      if (line == null) {
        if (ifDirectiveStates.Count != 0) {
          sb.AppendLine("#MalformedInput: missing #endif");
        }

        break;
      }

      string t = line.Trim();
      if (t.StartsWith("#if")) {
        IfDirectiveState rs = new IfDirectiveState(false, false);
        if (ignoreCutoff != -1) {
          // we're already in a state of ignoring, so continue to ignore
        } else if (IfdefConditionSaysToInclude(t.Substring(3).TrimStart(), defines)) {
          // include this branch
        } else {
          ignoreCutoff = ifDirectiveStates.Count; // start ignoring
          rs.mayStillIncludeAnotherAlternative = true; // allow some later "elsif" or "else" branch to be included
        }

        ifDirectiveStates.Add(rs);
        sb.Append(newline); // ignore the #if line
      } else if (t.StartsWith("#elsif")) {
        IfDirectiveState rs;
        if (ifDirectiveStates.Count == 0 || (rs = ifDirectiveStates[ifDirectiveStates.Count - 1]).hasSeenElse) {
          sb.Append("#MalformedInput: misplaced #elsif" + newline); // malformed input
          break;
        }

        if (ignoreCutoff == -1) {
          // we had included the previous branch
          //Contract.Assert(!rs.mayStillIncludeAnotherAlternative);
          ignoreCutoff = ifDirectiveStates.Count - 1; // start ignoring
        } else if (rs.mayStillIncludeAnotherAlternative &&
                   IfdefConditionSaysToInclude(t.Substring(6).TrimStart(), defines)) {
          // include this branch, but no subsequent branch at this level
          ignoreCutoff = -1;
          rs.mayStillIncludeAnotherAlternative = false;
          ifDirectiveStates[ifDirectiveStates.Count - 1] = rs;
        }

        sb.Append(newline); // ignore the #elsif line
      } else if (t == "#else") {
        IfDirectiveState rs;
        if (ifDirectiveStates.Count == 0 || (rs = ifDirectiveStates[ifDirectiveStates.Count - 1]).hasSeenElse) {
          sb.Append("#MalformedInput: misplaced #else" + newline); // malformed input
          break;
        }

        rs.hasSeenElse = true;
        if (ignoreCutoff == -1) {
          // we had included the previous branch
          //Contract.Assert(!rs.mayStillIncludeAnotherAlternative);
          ignoreCutoff = ifDirectiveStates.Count - 1; // start ignoring
        } else if (rs.mayStillIncludeAnotherAlternative) {
          // include this branch
          ignoreCutoff = -1;
          rs.mayStillIncludeAnotherAlternative = false;
        }

        ifDirectiveStates[ifDirectiveStates.Count - 1] = rs;
        sb.Append(newline); // ignore the #else line
      } else if (t == "#endif") {
        if (ifDirectiveStates.Count == 0) {
          sb.Append("#MalformedInput: misplaced #endif" + newline); // malformed input
          break;
        }

        ifDirectiveStates.RemoveAt(ifDirectiveStates.Count - 1); // pop
        if (ignoreCutoff == ifDirectiveStates.Count) {
          // we had ignored the branch that ends here; so, now we start including again
          ignoreCutoff = -1;
        }

        sb.Append(newline); // ignore the #endif line
      } else if (ignoreCutoff == -1) {
        sb.Append(line);
        sb.Append(newline);
      } else {
        sb.Append(newline); // ignore the line
      }
    }

    return sb.ToString();
  }

  public static string ReadLineAndDetermineNewline(TextReader reader, out string newline) {

    StringBuilder sb = new StringBuilder();
    newline = null;
    while (true) {
      int ch = reader.Read();
      if (ch == -1) {
        break;
      }

      if (ch == '\r' || ch == '\n') {
        if (ch == '\r') {
          if (reader.Peek() == '\n') {
            newline = "\r\n";
            reader.Read();
          } else {
            newline = "\r";
          }
        } else {
          newline = "\n";
        }

        return sb.ToString();
      }
      sb.Append((char)ch);
    }
    if (sb.Length > 0) {
      return sb.ToString();
    }

    return null;
  }
}