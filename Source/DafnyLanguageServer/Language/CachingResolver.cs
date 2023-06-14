using System.Collections.Generic;
using System.Security.Cryptography;
using Microsoft.Extensions.Logging;

namespace Microsoft.Dafny.LanguageServer.Language;

public class ResolutionCache {
  public PruneIfNotUsedSinceLastPruneCache<byte[], ModuleResolutionResult> Modules { get; } = new(new HashEquality());
  public BuiltIns? Builtins { get; set; }
  public Dictionary<TopLevelDeclWithMembers, Dictionary<string, MemberDecl>> SystemClassMembers { get; set; }

  public void Prune() {
    Modules.Prune();
  }
}
public class CachingResolver : ProgramResolver {
  private readonly ILogger<CachingResolver> logger;
  private readonly Dictionary<ModuleDecl, byte[]> hashes = new();
  private readonly ResolutionCache cache;
  
  
  public CachingResolver(Program program,
    ILogger<CachingResolver> logger,
    ResolutionCache cache)
    : base(program) {
    this.logger = logger;
    this.cache = cache;
  }

  protected override Dictionary<TopLevelDeclWithMembers, Dictionary<string, MemberDecl>> ResolveBuiltins(Program program) {
    if (cache.Builtins == null) {
      var systemClassMembers = base.ResolveBuiltins(program);
      cache.Builtins = program.BuiltIns;
      cache.SystemClassMembers = systemClassMembers;
    } else {
      program.BuiltIns = cache.Builtins;
    }

    return cache.SystemClassMembers;
  }

  protected override ModuleResolutionResult ResolveModuleDeclaration(CompilationData compilation, ModuleDecl decl) {
    var hash = DetermineHash(decl);
    hashes[decl] = hash;

    if (!cache.Modules.TryGet(hash, out var result)) {
      logger.LogDebug($"Resolution cache miss for {decl}");
      result = base.ResolveModuleDeclaration(compilation, decl);
      cache.Modules.Set(hash, result);
    } else {
      logger.LogDebug($"Resolution cache hit for {decl}");
    }

    // Clone declarations before returning them, since they are mutable and we don't want to mutate the one in the cache.
    // We should cache an immutable version of the AST instead: https://github.com/dafny-lang/dafny/issues/4086
    // var cloner = new Cloner(true, true);
    // var clonedResult = result! with {
    //   Signatures = new FileModuleDefinition(cloner, result.Module)
    // };
    return result!;
  }

  private byte[] DetermineHash(ModuleDecl moduleDeclaration) {
    var moduleVertex = dependencies.FindVertex(moduleDeclaration);
    var hashAlgorithm = HashAlgorithm.Create("SHA256")!;
    hashAlgorithm.Initialize();
    foreach (var dependencyVertex in moduleVertex.Successors) {
      var dependency = dependencyVertex.N;
      var dependencyHash = hashes[dependency];
      hashAlgorithm.TransformBlock(dependencyHash, 0, dependencyHash.Length, null, 0);
    }

    var parseHash = moduleDeclaration.CloneId.ToByteArray();
    hashAlgorithm.TransformFinalBlock(parseHash, 0, parseHash.Length);

    return hashAlgorithm.Hash!;
  }
}