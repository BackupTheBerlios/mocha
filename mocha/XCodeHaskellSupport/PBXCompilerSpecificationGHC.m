//
//  PBXCompilerSpecificationGHC.m
//  XCodeHaskellSupport
//
//  Created by André Pang on Wed Jul 30 2003.
//  Copyright (c) 2003 __MyCompanyName__. All rights reserved.
//

#import "PBXCompilerSpecification.h"
#import "PBXCompilerSpecificationGHC.h"

@implementation PBXCompilerSpecificationGHC : PBXCompilerSpecification

+ (void) load
{
    NSLog(@"PBXCompilerSpecificationGHC loading ...");
    NSLog(@"PBXCompilerSpecificationGcc reports "
          @"_specificationFilenameExtension as \"%@\"",
          [PBXCompilerSpecificationGcc _specificationFilenameExtension]);
    NSLog(@"PBXCompilerSpecificationGHC loaded.");    
}

- (id) bind
{
    NSLog(@"PBXCompilerSpecificationGHC: bind called");
    
    return nil;
}

- (id) inputFileTypes
{
    NSLog (@"PBXCompilerSpecificationGHC: inputFileTypes called");
    
    NSMutableArray *fileTypes = [[NSMutableArray alloc] init];
    [fileTypes addObject:@"hs"];
    [fileTypes addObject:@"haskell"];
    [fileTypes addObject:@"sourcecode.haskell"];
    
    return fileTypes;
}

@end
