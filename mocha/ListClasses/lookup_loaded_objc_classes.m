#include <stdlib.h>
#include <stdio.h>
#include <assert.h>

#include <objc/objc.h>
#include <objc/objc-runtime.h>

Class *registered_classes;
int num_of_registered_classes;
Class *current_class;
int current_class_index;

void initialise_class_lookup ()
{
  registered_classes = NULL;
  num_of_registered_classes = objc_getClassList(registered_classes, 0);
  printf("Found %d registered classes.\n", num_of_registered_classes);
  
  if (num_of_registered_classes > 0)
  {
    registered_classes = (Class *) calloc(num_of_registered_classes,
	sizeof(Class *));
    num_of_registered_classes = objc_getClassList (registered_classes,
	num_of_registered_classes);

    current_class = registered_classes;
    current_class_index = 0;
  }
}

const char *class_name (Class class)
{
  return class->name;
}

const char *superclass_name (Class class)
{
  if (class->super_class != NULL)
    return class->super_class->name;
  else
    return "(root class)";
}

struct objc_class *get_next_class ()
{
  if (current_class_index < num_of_registered_classes)
  {
    Class *c = current_class;
    current_class++;
    current_class_index++;
    return *c;
  }
  else
    return NULL;
}

void print_class_and_superclass_name (Class class)
{
  printf ("%s (%s)\n", class_name(class), superclass_name(class));
}

#if 0
int main(int argc, char **argv)
{
  Class c;

  initialise_class_lookup ();

  while ((c = get_next_class()) != NULL)
    print_class_and_superclass_name (c);

  return 0;
}
#endif

