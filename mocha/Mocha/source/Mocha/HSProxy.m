#include <stdlib.h>
#include <string.h>

#include <mach-o/dyld.h>
#include <objc/objc.h>
#include <objc/objc-class.h>
#include <objc/objc-runtime.h>

// #include <HsFFI.h>

#import <Foundation/Foundation.h>

#import "HSProxy.h"

@implementation HSProxy

+ (id) alloc
{
    NSLOG_RECEIVING_CLASS;

    return [super alloc];
}

+ (BOOL) proxyAsClass:(MochaInitialize)initializer
        withClassName:(char *)className
    andSuperclassName:(char *)superclassName
{
    NSLog(@"HSProxy: initialised with class name \"%s\", "
          @"super class name \"%s\" (initializer = %p)", className,
          superclassName, initializer);

    BOOL didCreateClass = [self   createClass:className
                           withSuperclassName:superclassName
                              withInitializer:initializer];

    if (didCreateClass == NO)
        NSLog(@"HSProxy: couldn't create class");

    char *initSymbolName = (char *) malloc (MAX_SYMBOLNAME_LEN * sizeof(char));
    assert (initSymbolName != NULL);
    strncpy (initSymbolName, GHC_STGINIT_PREFIX, MAX_SYMBOLNAME_LEN);
    strncat (initSymbolName, className, MAX_SYMBOLNAME_LEN);
    void *initSymbol;
    if (NSIsSymbolNameDefined (initSymbolName) == FALSE)
    {
        NSLog (@"Mocha: warning: Couldn't find %s", initSymbolName);
    }
    else
    {
        NSSymbol symbol = NSLookupAndBindSymbol (initSymbolName);
        initSymbol = NSAddressOfSymbol (symbol);
#if 0
        hs_add_root (initSymbol);
#endif
	NSSymbol hsAddRootSymbol = NSLookupAndBindSymbol
	    (SYMBOL_PREFIX "hs_add_root");
	hs_add_root_t hs_add_root = NSAddressOfSymbol (hsAddRootSymbol);
	hs_add_root (initSymbol);
#ifdef DEBUG
	NSLog (@"mocha: hs_add_root called");
#endif
    }
    free (initSymbolName);

    return YES;
}

+ (void)load
{
    static BOOL loadedHaskellObjects = NO;
    
    if (loadedHaskellObjects)
        return;

    [NSAutoreleasePool load];
    [NSAutoreleasePool initialize];
    NSAutoreleasePool *pool = [[NSAutoreleasePool alloc] init];

    /* initialise GHC runtime */
    
    int dummy_argc = 1;
    char **dummy_argv = calloc (2, sizeof(char *));
    dummy_argv[0] = "HSProxy";
    dummy_argv[1] = NULL;
#if 0
    hs_init (&dummy_argc, &dummy_argv);
    atexit (hs_exit);
#endif

#if 1
    {
	/* call hs_init */
	if (NSIsSymbolNameDefined (SYMBOL_PREFIX "hs_init") == FALSE)
	    NSLog (@"Mocha: couldn't find hs_init");
	NSSymbol symbol = NSLookupAndBindSymbol (SYMBOL_PREFIX "hs_init");
	hs_init_t initSymbol = NSAddressOfSymbol (symbol);
	initSymbol (&dummy_argc, &dummy_argv);
#ifdef DEBUG
	NSLog (@"mocha: hs_init called");
#endif

	/* ensure hs_exit gets run */
	if (NSIsSymbolNameDefined (SYMBOL_PREFIX "hs_exit") == FALSE)
	    NSLog (@"Mocha: couldn't find hs_exit");
	symbol = NSLookupAndBindSymbol (SYMBOL_PREFIX "hs_exit");
	hs_exit_t exitSymbol = NSAddressOfSymbol (symbol);
	atexit (exitSymbol);
#ifdef DEBUG
	NSLog (@"mocha: hs_exit registered with atexit");
#endif
    }
#endif
    free (dummy_argv);

    /* look for MochaClasses file path in the application bundle */
    
    NSString *mochaClassesPathNSString =
        [[NSBundle mainBundle] pathForResource:@"MochaClasses" ofType:nil];
    if (mochaClassesPathNSString == nil) return;

    char *mochaClassesPath = (char *) malloc(MAX_PATH_LEN * sizeof(char));
    assert (mochaClassesPath != NULL);
    [mochaClassesPathNSString getCString:mochaClassesPath
                               maxLength:MAX_PATH_LEN];

    /* read in class names from MochaClasses file */
    
    FILE *mochaClasses = fopen (mochaClassesPath, "r");

    char *className = (char *) malloc(MAX_CLASSNAME_LEN * sizeof(char));
    assert (className != NULL);
    while ((className = fgets(className, MAX_CLASSNAME_LEN, mochaClasses))
           != NULL)
    {
        MochaInitialize initializer;
        MochaSuperclassName getSuperclassName;
        
        /* get rid of the \n */
        char *newline = strchr (className, '\n');
        if (newline != NULL) *newline = '\0';

        /* look for the class' symbol */
        char *symbolName = (char *) malloc (MAX_SYMBOLNAME_LEN *
                                            sizeof(char));
        assert (symbolName != NULL);
        strncpy (symbolName, MOCHA_CLASS_PREFIX, MAX_SYMBOLNAME_LEN);
        strncat (symbolName, className, MAX_SYMBOLNAME_LEN);

        if (NSIsSymbolNameDefined(symbolName) == FALSE)
        {
            NSLog(@"Mocha: warning: Couldn't find the Haskell class named"
                  @"\"%s\"", className);
        }
        else
        {
            NSSymbol symbol = NSLookupAndBindSymbol (symbolName);
            initializer = NSAddressOfSymbol (symbol);
#ifdef DEBUG
            NSLog(@"Mocha: Found class \"%s\" at %p", className, initializer);
#endif
        }

        /* look for the superclass' symbol */
        char *superSymbolName = (char *) malloc (MAX_SYMBOLNAME_LEN *
                                                 sizeof(char));
        assert (superSymbolName != NULL);
        strncpy (superSymbolName, MOCHA_SUPERCLASSNAME_PREFIX,
                 MAX_SYMBOLNAME_LEN);
        strncat (superSymbolName, className, MAX_SYMBOLNAME_LEN);
        if (NSIsSymbolNameDefined(superSymbolName) == FALSE)
        {
            NSLog(@"Mocha: warning: Couldn't find superclass name for the"
                  @"class named \"%s\"", className);
        }
        else
        {
            NSSymbol superSymbol = NSLookupAndBindSymbol (superSymbolName);
            getSuperclassName = NSAddressOfSymbol (superSymbol);
#ifdef DEBUG
            NSLog (@"Mocha: Found superclass name for \"%s\" at %p",
                   className, getSuperclassName);
#endif
        }

        [HSProxy proxyAsClass:initializer
                withClassName:className
            andSuperclassName:getSuperclassName()];

        free (superSymbolName);
        free (symbolName);
    }

    [pool release];
    free (mochaClassesPath);
    free (className);

    loadedHaskellObjects = TRUE;
}

- (id) init
{
    NSLOG_RECEIVING_CLASS;

    if (RECEIVER_NOT_HSPROXY)
    {
        struct objc_class *classObject = [self class];
        MochaInitialize initializer = (MochaInitialize) classObject->version;
        initializer (self);
#ifdef DEBUG
        NSLog(@"HSProxy: post-initialisation is %p %p %p", messageHandler,
              classRespondsToSelector, methodSignatureForSelector);
#endif
    }
    else
    {
        return nil;
    }

    return self;
}

#if 0
- (BOOL)willForwardSelector:(SEL)selector
{
    NSLog (@"HSProxy: willForwardSelector for \"%s\" called",
           sel_getName(selector));

    return YES;
}
#endif

- (void)forwardInvocation:(NSInvocation *)anInvocation
{
    NSLOG_RECEIVING_CLASS;
    
    NSLog (@"NSInvocation received: \"%@\"", anInvocation);

    id invocationTarget;
    SEL invocationSelector;

    // Method signature?
    int args = [[anInvocation methodSignature] numberOfArguments];
    NSLog (@"Invocation number of arguments is %d", args);

    [anInvocation getArgument:&invocationTarget   atIndex:0];
    [anInvocation getArgument:&invocationSelector atIndex:1];

    NSLog (@"Invocation details: target:%p selector:\"%s\"",
           invocationTarget, sel_getName (invocationSelector));

    struct objc_class *invocationTargetClass = [invocationTarget class];
    NSLog (@"Invocation target class: \"%s\"", invocationTargetClass->name);

    messageHandler (anInvocation);
}

- (NSMethodSignature *)methodSignatureForSelector:(SEL)aSelector
{
    NSLOG_RECEIVING_CLASS;
    
    const char *selectorCString = sel_getName(aSelector);

    NSLog (@"methodSignatureForSelector:\"%s\" called", selectorCString);

    struct objc_class *selfClass;
    selfClass = [self class];

    if (RECEIVER_NOT_HSPROXY)
    {
        NSLog(@"HSProxy: methodSignatureForSelector called for \"%s\"",
              selfClass->name);
        
        char *methodSignature =
            methodSignatureForSelector(sel_getName(aSelector));

#ifdef DEBUG
	NSLog (@"HSProxy: methodSignatureForSelector retruned \"%s\"",
	      methodSignature);
#endif

        if (strcmp(methodSignature, "XXX") != 0)
            return [NSMethodSignature signatureWithObjCTypes:methodSignature];
        else
            return [super methodSignatureForSelector:aSelector];
    }
    else
    {
        NSMethodSignature *sig = [super methodSignatureForSelector:aSelector];
        NSLog(@"methodSignatureForSelector: superclass invoked, %@",
              sig);
        return sig;
    }
        
}

- (BOOL)respondsToSelector:(SEL)aSelector
{
    NSLOG_RECEIVING_CLASS;
    
    NSLog (@"respondsToSelector:\"%s\" called", sel_getName(aSelector));

    struct objc_class *selfClass;
    selfClass = [self class];
    NSLog(@"HSProxy: respondsToSelector called for \"%s\"", selfClass->name);
    if (strcmp(selfClass->name, "HSProxy") != 0)
    {
        if (classRespondsToSelector == NULL)
        {
            NSLog(@"HSProxy: classRespondsToSelector is NULL");
            return NO;
        }

        BOOL doesRespondToSelector =
            classRespondsToSelector(sel_getName(aSelector));
                                                             
        if (!doesRespondToSelector)
        {
#ifdef DEBUG
            NSLog(@"HSProxy: Prinicipal class doesn't respond to selector, "
                  @"forwarding to superclass");
#endif
            return [super respondsToSelector:aSelector];
        }
        else
        {
#ifdef DEBUG
            NSLog(@"HSProxy: Does respond to selector");
#endif
            return doesRespondToSelector;
        }
    }
    else
    {
        return [super respondsToSelector:aSelector];
    }
}

- (BOOL) isProxy
{
    NSLOG_RECEIVING_CLASS;
    
    return YES;
}

- (Class) superclass
{
    NSLOG_RECEIVING_CLASS;
    
    return [super superclass];
}

- (BOOL)isKindOfClass:(Class)aClass
{
    NSLOG_RECEIVING_CLASS;
    
    struct objc_class *class = (struct objc_class *) aClass;

    NSLog (@"isKindOfClass:%p called", class);
    NSLog (@"isKindOfClass: class object is \"%s\"", class->name);

    return [super isKindOfClass:aClass];
}

- (BOOL)isMemberOfClass:(Class)aClass
{
    struct objc_class *selfClass = (struct objc_class *) [self class];
    struct objc_class *class = (struct objc_class *) aClass;

    NSLog (@"isMemberOfClass: class object is \"%s\", "
           @"self class object is \"%s\"", class->name, selfClass->name);

    if (strcmp(class->name, selfClass->name) == 0)
        return YES;
    else
        return NO;
}

- (void) initTheHaskellClass:(MessageHandler)mh
                     withRTS:(RespondsToSelector)rts
                    withMSFS:(MethodSignatureForSelector)msfs
{
    messageHandler = mh;
    classRespondsToSelector = rts;
    methodSignatureForSelector = msfs;
}

+ (BOOL) createClass:(char *)name
  withSuperclassName:(char *)superclassName
     withInitializer:(MochaInitialize)initializer
{
    /* 
     * From the objc_addClass documentation in "The Objective-C
     * Programming Language" by Apple
     */

    struct objc_class * meta_class;
    struct objc_class * super_class;
    struct objc_class * new_class;
    struct objc_class * root_class;

    // HSProxy class object
    struct objc_class *_HSProxy_ = [HSProxy class];

    // Ensure that the superclass exists and that someone
    // hasn't already implemented a class with the same name

    super_class = (struct objc_class *)objc_lookUpClass (superclassName);
    if (super_class == nil)
        return NO;

    if (objc_lookUpClass (name) != nil) 
        return NO;

    // Find the root class
    root_class = super_class;
    while( root_class->super_class != nil )
	root_class = root_class->super_class;

    // Allocate space for the class and its meta class
    new_class = calloc( 2, sizeof(struct objc_class) );
    meta_class = &new_class[1];

    // Set the class's version to be the message handler pointer (dooodgey)
    new_class->version = (int) initializer;
    meta_class->version = (int) initializer;

    // setup class
    new_class->isa      = meta_class;
    new_class->info     = CLS_CLASS;
    meta_class->info    = CLS_META;

    //
    // Create a copy of the class name.
    // For efficiency, we have the metaclass and the class itself 
    // to share this copy of the name, but this is not a requirement
    // imposed by the runtime.
    //

    new_class->name = malloc (strlen (name) + 1);
    strcpy ((char*)new_class->name, name);
    meta_class->name = new_class->name;

    //
    // Allocate empty method lists
    // We can add methods later.
    //

    new_class->info |= _HSProxy_->info & CLS_METHOD_ARRAY;
    meta_class->info |= _HSProxy_->isa->info & CLS_METHOD_ARRAY;
    
    new_class->methodLists = _HSProxy_->methodLists;
    meta_class->methodLists = _HSProxy_->isa->methodLists;
    

    // Connect the class definition to the class hierarchy.
    // 
    // First, connect the class to the superclass
    // Then connect the metaclass to the metaclass of the superclass
    // Then connect the metaclass of the metaclass to
    //                                  the metaclass of the root class

    new_class->super_class  = super_class;
    meta_class->super_class = super_class->isa;
    meta_class->isa         = (void *)root_class->isa;

    // Finally, register the class with the runtime.
    objc_addClass( new_class ); 
    return YES;

}

@end

void __mocha_initialiseSurrogate (HSProxy *surrogate, MessageHandler mh,
                                  RespondsToSelector rts,
                                  MethodSignatureForSelector msfs)
{
    surrogate->messageHandler = mh;
    surrogate->classRespondsToSelector = rts;
    surrogate->methodSignatureForSelector = msfs;
}

