#include <stdio.h>
#include <objc/objc-class.h>

#include "MochaBridge.h"

#import <Foundation/Foundation.h>

int main (int argc, char **argv)
{
  struct objc_class *co = (struct objc_class *) 0xa07ec7e8;
  fprintf (stderr, "name:%s initialized:%ld\n", co->name,
    ((co->info) & CLS_NEED_BIND));
  id classObject = [NSArray class];
  fprintf (stderr, "name:%s initialized:%ld\n", co->name,
    ((co->info) & CLS_NEED_BIND));
  NSLog (@"classObject = %p", classObject);
  const char *s = returnTypeEncoding (classObject, "arrayWithArray:");
  NSLog (@"returnTypeEncoding = %s", s);
  
  id foo = [NSURL URLWithString:@"http://foo.com/bar/"];
  //id foo = [[NSNumber alloc] initWithInt:69];
  NSLog (@"foo points to %p", foo);
  struct objc_class *foo_co = (struct objc_class *) foo->isa;
  struct objc_class *foo_super = foo_co->super_class->super_class;
  fprintf (stderr, "CLS_CLASS is %ld\n", foo_co->info & CLS_CLASS);
  fprintf (stderr, "CLS_META is %ld\n", foo_co->info & CLS_META);
  fprintf (stderr, "super CLS_CLASS is %ld\n", foo_super->info & CLS_CLASS);
  fprintf (stderr, "super CLS_META is %ld\n", foo_super->info & CLS_META);
  fprintf (stderr, "classObjectName = %s\n", foo_co->name);
  fprintf (stderr, "superclassObjectName = %s\n", foo_super->name);
  fprintf (stderr, "instance_size = %ld\n", foo_co->instance_size);

  struct objc_ivar_list *ivl = foo_co->super_class->ivars;
  if (ivl == NULL)
  {
     NSLog (@"class has no instance variables");
  }
  else
  {
     int i;

     NSLog (@"ivl points to %p", ivl);
     NSLog (@"%d ivars in class:", ivl->ivar_count);
     for (i = 0; i < ivl->ivar_count; i++) 
     {
	NSLog (@"ivar i is (%s, %s, %d)",
	  ivl->ivar_list[i].ivar_name,
	  ivl->ivar_list[i].ivar_type,
	  ivl->ivar_list[i].ivar_offset);
     }
  }

  NSString *urlString = foo_co->super_class + sizeof(struct objc_class);
  NSLog (@"urlString is \"%@\"", urlString);

  return 0;
}

