#include <objc/objc.h>

typedef void ObjCMessageExpression;

char *argumentsTypeEncoding (id receiver, const char *methodName);
char *returnTypeEncoding (id receiver, const char *methodName);

ObjCMessageExpression *makeMessageExpression
  (id receiver, const char *methodName);

/*
void setArgumentToShort (int index, ObjCMessageExpression *expr, short argument);
void setArgumentToLong (int index, ObjCMessageExpression *expr, long argument);
void setArgumentToLongLong (int index, ObjCMessageExpression *expr, long long argument);
void setArgumentToUnsignedChar (int index, ObjCMessageExpression *expr, unsigned char argument);
void setArgumentToUnsignedInt (int index, ObjCMessageExpression *expr, unsigned int argument);
void setArgumentToUnsignedShort (int index, ObjCMessageExpression *expr, unsigned short argument);
void setArgumentToUnsignedLong (int index, ObjCMessageExpression *expr, unsigned long argument);
void setArgumentToUnsignedLongLong (int index, ObjCMessageExpression *expr, unsigned long long argument);
void setArgumentToFloat (int index, ObjCMessageExpression *expr, float argument);
void setArgumentToDouble (int index, ObjCMessageExpression *expr, double argument);
void // void
void setArgumentToCString (int index, ObjCMessageExpression *expr, char *argument);
void setArgumentToObject (int index, ObjCMessageExpression *expr, id argument);
void setArgumentToClassObject (int index, ObjCMessageExpression *expr, Class argument);
void //setArgumentToArray (int index, ObjCMessageExpression *expr, void *argument);
void //setArgumentToStruct (int index, ObjCMessageExpression *expr, char argument);
void //setArgumentToUnion (int index, ObjCMessageExpression *expr, char argument);
void setArgumentToPtr (int index, ObjCMessageExpression *expr, void *argument);
*/

void setCharArgument (int index, ObjCMessageExpression *expr, char argument);
void setIntArgument (int index, ObjCMessageExpression *expr, int argument);
void setBoolArgument (int index, ObjCMessageExpression *expr, BOOL argument);
void setVoidPtrArgument (int index, ObjCMessageExpression *expr,
			 void *argument);
void setShortArgument (int index, ObjCMessageExpression *expr, short argument);
void setLongArgument (int index, ObjCMessageExpression *expr, long argument);
void setLongLongArgument (int index, ObjCMessageExpression *expr,
    			  long long argument);
void setFloatArgument (int index, ObjCMessageExpression *expr, float argument);
void setDoubleArgument (int index, ObjCMessageExpression *expr, double argument);
void setStringArgument (int index, ObjCMessageExpression *expr, char *argument);
void setIDArgument (int index, ObjCMessageExpression *expr, id argument);
void setClassArgument (int index, ObjCMessageExpression *expr, Class argument);
void setSelectorArgument (int index, ObjCMessageExpression *expr, SEL argument);

id getIDArgument (int index, ObjCMessageExpression *expr);

void marshalCStringAsNSString (int index, ObjCMessageExpression *expr, char *argument);

char *marshalNSStringToCString (id object);

void sendVoidMessage (ObjCMessageExpression *expr);
int sendIntMessage (ObjCMessageExpression *expr);
id sendIDMessage (ObjCMessageExpression *expr);
char sendBoolMessage (ObjCMessageExpression *expr);
char *sendSelectorMessage (ObjCMessageExpression *expr);
char *sendCStringMessage (ObjCMessageExpression *expr);

void pushAutoreleasePool ();
void popAutoreleasePool ();

