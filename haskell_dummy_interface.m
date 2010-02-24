#import <Cocoa/Cocoa.h>
#import "FFI.h"
#import "HSObjC_C.h"

void freeStablePtr(HsStablePtr aStablePointer)
{
    return;
}

HsStablePtr newStableIdContainer(id someObject)
{
    return NULL;
}

id retrieveId(HsStablePtr aStablePointer)
{
    return nil;
}


HSValue *newStoredArray(id someObject)
{
    return nil;
}

id retrieveStoredArray(HSValue *aStablePointer)
{
    return nil;
}

id getFunctionList(void)
{
    return nil;
}


int main(int argc, char *argv[])
{
    return NSApplicationMain(argc,  (const char **) argv);
}
