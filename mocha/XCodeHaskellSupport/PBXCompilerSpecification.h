@interface PBXCompilerSpecification
{
    NSMutableArray *_inputFileTypes;
}

+ (Class)_specificationBaseClass;
+ _localizedSpecificationTypeName;
+ _specificationRegistry;
+ _specificationFilenameExtension;
+ _specialPropertyKeys;
+ displaySpecifications;
- initWithPropertyListDictionary:fp8;
- (void)dealloc;
- inputFileTypes;
- (char)acceptsInputFileType:fp8;
- (char)isAbstract;
- builtinJambaseRuleName;
- jamfileCodeString;
- effectiveCompilerSpecificationForFileNamed:fp8 inTargetBuildContext:fp12;
- computeDependenciesForFilePaths:fp8 outputDirectory:fp12 inTargetBuildContext:fp16;
- _defaultOutputDirectory;
- _uniqueOutputBaseNameForInputFilePath:fp8 inTargetBuildContext:fp12;
- _outputFilesForInputFilePath:fp8 inTargetBuildContext:fp12;

@end

@interface PBXCompilerSpecificationGcc : PBXCompilerSpecification
{
}

- _systemGccVersionString;
- name;
- effectiveCompilerSpecificationForFileNamed:fp8 inTargetBuildContext:fp12;
- computeDependenciesForFilePaths:fp8 outputDirectory:fp12 inTargetBuildContext:fp16;
          
@end

struct PBXCompilerSpecificationGccDef {
    @defs(PBXCompilerSpecificationGcc)
} PBXCompilerSpecificationGccInstanceVariables;
