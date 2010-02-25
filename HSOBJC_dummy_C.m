#import <Cocoa/Cocoa.h>
#import "FFI.h"

id callFunc1(HsStablePtr func, id arg1)
{
    return nil;
}

id callFunc2(HsStablePtr func, id arg1, id arg2)
{
    return nil;
}

void freeStablePtr(HsStablePtr aStablePointer)
{
    return;
}

id initController(NSDictionary *ivars)
{
    return nil;
}

int main(int argc, char *argv[])
{
    return NSApplicationMain(argc,  (const char **) argv);
}
