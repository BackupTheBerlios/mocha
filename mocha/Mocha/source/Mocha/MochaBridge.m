#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <objc/objc.h>
#include <objc/objc-runtime.h>

#import <Foundation/NSArray.h>
#import <Foundation/NSAutoreleasePool.h>
#import <Foundation/NSInvocation.h>
#import <Foundation/NSMethodSignature.h>
#import <Foundation/NSString.h>

#include <HsFFI.h>

#include "MochaBridge.h"

#undef MOCHA_DEBUG

Class *GetClassList ();

static NSMutableArray *autoreleasePoolStack = NULL;

char *argumentsTypeEncoding (id receiver, const char *methodName)
{
  int i;
  int typeEncodingLength;
  char *typeEncoding;

  typeEncodingLength = 0;
  
  SEL s = sel_registerName (methodName);
  NSMethodSignature *methodSignature = [receiver methodSignatureForSelector:s];
  if (methodSignature == nil)
  {
      NSLog (@"methodSignatureForSelector(%s) for class %s failed",
             methodName, ((struct objc_class *) [receiver class])->name);
      assert (nil);
  }
  int numberOfArguments = [methodSignature numberOfArguments] - 2;
  
  /* find out how long the final type encoding string will be */
  for (i = 2; i < numberOfArguments + 2; i++)
  {
    const char *str = [methodSignature getArgumentTypeAtIndex:i];
    typeEncodingLength += strlen(str);
  }

  /* get the type encoding for all of the method's arguments */
  typeEncoding = (char *) calloc ((typeEncodingLength + 1), sizeof(char));
  assert (typeEncoding != NULL);

  for (i = 2; i < numberOfArguments + 2; i++)
  {
    const char *s = [methodSignature getArgumentTypeAtIndex:i];
    strcat (typeEncoding, s);
  }

#ifdef MOCHA_DEBUG
  NSLog(@"argumentsTypeEncoding: methodName=%s, numberOfArguments=%d "
        @"(typeEncoding=%s)", methodName, numberOfArguments, typeEncoding);
#endif

  /* remove constness from the type signature to kill a warning */
  return (char *) typeEncoding;
}

char *returnTypeEncoding (id receiver, const char *methodName)
{
  SEL s = sel_registerName (methodName);

  NSMethodSignature *methodSignature =
    [receiver methodSignatureForSelector:s];
  assert (methodSignature != NULL);

  /* remove constness from the type signature to kill a warning */
  return (char *) [methodSignature methodReturnType];
}

ObjCMessageExpression *makeMessageExpression
  (id receiver, const char *methodName)
{
   NSAutoreleasePool *pool = [[NSAutoreleasePool alloc] init];

   NSInvocation *invocation;
   NSMethodSignature *methodSignature;
   SEL selector = sel_registerName (methodName);

   methodSignature = [receiver methodSignatureForSelector:selector];
   [methodSignature retain];
   invocation = [NSInvocation invocationWithMethodSignature:methodSignature];
   [invocation retain];
   [invocation setSelector:selector];
   [invocation setTarget:receiver];

   [pool release];

   return invocation;
}

#define SET_ARGUMENT \
   NSAutoreleasePool *pool = [[NSAutoreleasePool alloc] init]; \
   NSInvocation *invocation = (NSInvocation *) expr; \
   [invocation setArgument:&argument atIndex:index+1]; \
   [pool release];

char *marshalNSStringToCString (id object)
{
   NSString *s = (NSString *) object;
   
   unsigned int cStringLength = [s cStringLength];
   char *cString = (char *) malloc (cStringLength * sizeof(char));
   [s getCString:cString];

#ifdef MOCHA_DEBUG
   NSLog(@"marshalNSStringToCString: NSString is \"%@\", cString is \"%s\"", s, cString);
#endif

   return cString;
}

void marshalCStringAsNSString (int index, ObjCMessageExpression *expr, char *unmarshaledArgument)
{
   NSAutoreleasePool *pool2 = [[NSAutoreleasePool alloc] init];
   NSString *argument = [NSString stringWithCString:unmarshaledArgument];

#ifdef MOCHA_DEBUG
   NSLog(@"marshalCStringAsNSString: %@", argument);
#endif

   [argument retain];
   [pool2 release];

   SET_ARGUMENT
}

void setCharArgument (int index, ObjCMessageExpression *expr, char argument)
{
  SET_ARGUMENT
}

void setIntArgument (int index, ObjCMessageExpression *expr, int argument)
{
   SET_ARGUMENT
}

void setBoolArgument (int index, ObjCMessageExpression *expr, BOOL argument)
{
   SET_ARGUMENT
}

void setVoidPtrArgument (int index, ObjCMessageExpression *expr,
			 void *argument)
{
   SET_ARGUMENT
}

void setShortArgument (int index, ObjCMessageExpression *expr, short argument)
{
   SET_ARGUMENT
}

void setLongArgument (int index, ObjCMessageExpression *expr, long argument)
{
   SET_ARGUMENT
}

void setLongLongArgument (int index, ObjCMessageExpression *expr,
                          long long argument)
{
   SET_ARGUMENT
}

void setFloatArgument (int index, ObjCMessageExpression *expr, float argument)
{
   SET_ARGUMENT
}

void setDoubleArgument (int index, ObjCMessageExpression *expr, double argument)
{
   SET_ARGUMENT
}

void setStringArgument (int index, ObjCMessageExpression *expr, char *argument)
{
   SET_ARGUMENT
}

void setIDArgument (int index, ObjCMessageExpression *expr, id argument)
{
   SET_ARGUMENT
}

void setClassArgument (int index, ObjCMessageExpression *expr, Class argument)
{
   SET_ARGUMENT
}

void setSelectorArgument (int index, ObjCMessageExpression *expr, SEL argument)
{
  SET_ARGUMENT
}

id getIDArgument (int index, ObjCMessageExpression *expr)
{
    id buffer;
    
    NSAutoreleasePool *pool = [[NSAutoreleasePool alloc] init];
    NSInvocation *invocation = (NSInvocation *) expr;
    [invocation getArgument:&buffer atIndex:index+1];
    [pool release];

    return buffer;
}


/*
 * Message sending
 */

void sendVoidMessage (ObjCMessageExpression *expr)
{
   NSAutoreleasePool *pool = [[NSAutoreleasePool alloc] init];
   NSInvocation *invocation = (NSInvocation *) expr;
   [invocation invoke];
   [pool release];
}

int sendIntMessage (ObjCMessageExpression *expr)
{
   NSAutoreleasePool *pool = [[NSAutoreleasePool alloc] init];

   NSInvocation *invocation = (NSInvocation *) expr;

   unsigned int returnLength = [[invocation methodSignature] 
                                methodReturnLength];
   [invocation invoke];
   int *buffer = (int *) malloc (returnLength);
   [invocation getReturnValue:buffer];
   
   [pool release];

   return *buffer;
}

char sendBoolMessage (ObjCMessageExpression *expr)
{
   NSAutoreleasePool *pool = [[NSAutoreleasePool alloc] init];

   NSInvocation *invocation = (NSInvocation *) expr;

   unsigned int returnLength = [[invocation methodSignature] 
                                methodReturnLength];
   [invocation invoke];
   char *buffer = (char *) malloc (returnLength);
   [invocation getReturnValue:buffer];
   
   [pool release];

   return *buffer;
}

id sendIDMessage (ObjCMessageExpression *expr)
{
   NSAutoreleasePool *pool = [[NSAutoreleasePool alloc] init];

   NSInvocation *invocation = (NSInvocation *) expr;
   [invocation invoke];

   id object;
   [invocation getReturnValue:&object];
   
   [pool release];

   return object;
}

char *sendSelectorMessage (ObjCMessageExpression *expr)
{
   NSAutoreleasePool *pool = [[NSAutoreleasePool alloc] init];

   NSInvocation *invocation = (NSInvocation *) expr;

   unsigned int returnLength = [[invocation methodSignature] 
                                methodReturnLength];
   [invocation invoke];
   SEL *buffer = (SEL *) malloc (returnLength);
   [invocation getReturnValue:buffer];
   char *buffer2 = (char *) sel_getName(*buffer);

   [pool release];

   return buffer2;
}

char *sendCStringMessage (ObjCMessageExpression *expr)
{
   NSAutoreleasePool *pool = [[NSAutoreleasePool alloc] init];

   NSInvocation *invocation = (NSInvocation *) expr;

   unsigned int returnLength = [[invocation methodSignature] 
                                methodReturnLength];
   [invocation invoke];
   char **buffer = (char **) malloc (returnLength);
   [invocation getReturnValue:buffer];

   [pool release];

   return *buffer;
}

Class *GetClassList ()
{
    Class *registeredClasses;
    int numberOfRegisteredClasses;

    numberOfRegisteredClasses = objc_getClassList (registeredClasses, 0);
    if (numberOfRegisteredClasses > 0)
    {
	registeredClasses = (Class *) calloc (numberOfRegisteredClasses,
		sizeof(Class *));
	numberOfRegisteredClasses = objc_getClassList (registeredClasses,
		numberOfRegisteredClasses);
	return registeredClasses;
    }
    else
    {
	return NULL;
    }
}


@interface HSMochaAutoreleasePool : NSAutoreleasePool
- (void) retain;
@end

@implementation HSMochaAutoreleasePool : NSAutoreleasePool
- (void) retain
{
    // ignore retain message
    return;
}
@end

void pushAutoreleasePool ()
{
    HSMochaAutoreleasePool *pool = [[HSMochaAutoreleasePool alloc] init];
    if (autoreleasePoolStack == NULL)
	autoreleasePoolStack = [[NSMutableArray alloc] init];
    [autoreleasePoolStack addObject:pool];
}

void popAutoreleasePool ()
{
    if (autoreleasePoolStack != NULL)
    	[autoreleasePoolStack removeLastObject];
}

