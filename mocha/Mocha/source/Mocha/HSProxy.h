#include <objc/objc.h>

/*
 * Haskell object callbacks
 */
typedef void   (*MessageHandler) (id /* NSInvocation * */);
typedef BOOL   (*RespondsToSelector) (const char *);
typedef char * (*MethodSignatureForSelector) (const char *);
typedef void   (*MochaInitialize) (id);
typedef char * (*MochaSuperclassName) (void);

typedef void   (*hs_init_t) (int *argc, char **argv[]);
typedef void   (*hs_exit_t) (void);
typedef void   (*hs_add_root_t) (void *);

/*
 * Declare NSMethodSignature's secrets ...
 */

#ifdef __OBJC__

@interface NSMethodSignature (HSProxy)
+ (id)signatureWithObjCTypes:(const char *)typeEncoding;
@end

@interface HSProxy : NSObject
{
    @public
    MessageHandler messageHandler;
    RespondsToSelector classRespondsToSelector;
    MethodSignatureForSelector methodSignatureForSelector;

    @protected
    char *principalClassName;
    char *principalSuperclassName;
}

+ (BOOL) proxyAsClass:(MochaInitialize)initializer
        withClassName:(char *)className
    andSuperclassName:(char *)superclassName;

- (void) initTheHaskellClass:(MessageHandler)mh
                     withRTS:(RespondsToSelector)rts
                    withMSFS:(MethodSignatureForSelector)msfs;

+ (BOOL) createClass:(char *)name
  withSuperclassName:(char *)superclassName
     withInitializer:(MochaInitialize)initializer;

@end

#else

typedef void HSProxy;

#endif

/*
 * This initialisation function merely sets instance variables in the
 * surrogate object.  It must be in a C function instead of being in
 * the HSProxy class because the Haskell object will call this function
 * while HSProxy is still in its "- init" method, which means that
 * it isn't initialised yet.  Thus, attempting to invoke any methods on
 * the object won't work: the object must be fully initialised first.
 *
 * Note that this is a global function, so that the Haskell runtime
 * can call back into it via a "foreign import".
 */
void __mocha_initialiseSurrogate (HSProxy *surrogate, MessageHandler mh,
                                  RespondsToSelector rts,
                                  MethodSignatureForSelector msfs);

/*
 * Macros
 */

/* Lengths */

#ifndef MAX_PATH_LEN
#define MAX_PATH_LEN 1024
#endif

#define MAX_CLASSNAME_LEN 1024

#define MAX_SYMBOLNAME_LEN 1024

/* Mac OS X has a leading underscore for its symbol prefix*/
#define SYMBOL_PREFIX "_"

#define MOCHA_CLASS_PREFIX (SYMBOL_PREFIX "__mocha_class_")
#define MOCHA_SUPERCLASSNAME_PREFIX (SYMBOL_PREFIX "__mocha_superclassName_")

#define GHC_STGINIT_PREFIX (SYMBOL_PREFIX "__stginit_")

#define SELF_CLASSNAME (((struct objc_class *) [self class])->name)

#define RECEIVER_IS_HSPROXY (strcmp(SELF_CLASSNAME, "HSProxy") == 0)
#define RECEIVER_NOT_HSPROXY (!RECEIVER_IS_HSPROXY)


/*
 * Debugging macros
 */

#ifndef DEBUG
#define DEBUG
#endif

/* Given a function name, prints out what the receiving class is for
 * this function */
#ifdef DEBUG
#define NSLOG_RECEIVING_CLASS \
NSLog (@"HSProxy: [%s %s] invoked", SELF_CLASSNAME, sel_getName(_cmd));
#else
#define NSLOG_RECEIVING_CLASS do {} while (0);
#endif

