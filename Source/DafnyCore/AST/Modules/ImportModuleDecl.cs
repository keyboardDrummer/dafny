using System;

namespace Microsoft.Dafny;

public abstract class ImportModuleDecl : ModuleDecl {
  public readonly ModuleQualifiedId TargetQId; // generated by the parser, this is looked up

  protected ImportModuleDecl(Cloner cloner, ImportModuleDecl original, ModuleDefinition parent) :
    base(cloner, original, parent) {
    if (original.TargetQId != null) { // TODO is this null check necessary?
      TargetQId = new ModuleQualifiedId(cloner, original.TargetQId);

      /*
       * Refinement cloning happens in PreResolver, which is after the ModuleQualifiedId.Root fields are set,
       * so this field must be copied as part of refinement cloning.
       * However, letting refinement cloning set CloneResolvedFields==true causes exceptions for an uninvestigated reason,
       * so we will clone this field even when !CloneResolvedFields.
       */
      TargetQId.Root = original.TargetQId.Root;
    }
  }

  protected ImportModuleDecl(DafnyOptions options, RangeToken rangeToken, ModuleQualifiedId path, Name name,
    ModuleDefinition parent, bool opened, bool isRefining, Guid cloneId)
    : base(options, rangeToken, name, parent, opened, isRefining, cloneId) {
    TargetQId = path;
  }
}