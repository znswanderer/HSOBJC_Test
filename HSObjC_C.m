// ================================================================
// Copyright (C) 2010 Tim Scheffler
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
// http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.
// ================================================================


#include "HSObjC_C.h"

void releaseId(id object)
{
    // release can trigger dealloc, which might need an autorelease pool
    NSAutoreleasePool *pool = [[NSAutoreleasePool alloc] init];
    [object release];
    [pool release];
}

id retainId(id object)
{
    return [object retain];
}

id autoreleaseId(id object)
{
    return [object autorelease];
}

id performMethod0(const char* methodName, id object)
{
    SEL theSelector = sel_registerName(methodName);
    return [object performSelector:theSelector];
}

id performMethod1(const char* methodName, id object, id arg1)
{
    SEL theSelector = sel_registerName(methodName);
    return [object performSelector:theSelector withObject:arg1];
}


// NSString conversion

const char *nsStringToUtf8(NSString *str)
// returns a CString of UTF8 chars for the NSString str.
// if str is not a NSString, it will pass back NULL
{
    // it is quite time consuming to test at runtime if `str` really is a NSString.
    // so we just be optimistic and deal with an exception by passing back the null pointer.
    const char *p;
    @try {
        // according to the documentation of NSString's UTF8String the resulting
        // char array will be freed by the autorelease pool.
        p = [str UTF8String];
    }
    @catch (NSException * e) {
        p = NULL;
    }
    
    return p;
}

NSString *utf8ToNSString(const char* cstr)
{
    NSString *res;
    @try {
        res = [NSString stringWithUTF8String:cstr];
    }
    @catch (NSException * e) {
        res = NULL;
    }
    
    return res;
}


// NSNumber handling
int isNSNumber(id object)
{
    if ([object isKindOfClass:[NSNumber class]]) 
        return 1;
    else 
        return 0;
}

double doubleValue(NSNumber *aNumber)
{
    return [aNumber doubleValue];
}

NSNumber *numberWithDouble(double aDouble)
{
    return [NSNumber numberWithDouble:aDouble];
}

long longValue(NSNumber *aNumber)
{
    return [aNumber longValue];
}

NSNumber *numberWithLong(long aLong)
{
    return [NSNumber numberWithLong:aLong];
}


// NSArray handling
NSArray *arrayWithCArray(id *objects, NSUInteger count)
{
    return [NSArray arrayWithObjects:objects count:count];
}

NSUInteger lengthOfArray(NSArray *anArray)
{
    NSUInteger len;
    @try {
        len = [anArray count];
    }
    @catch (NSException * e) {
        len = 0;
    }
    return len;
}

id *getObjects(NSArray *anArray)
// RESULTING ARRAY MUST BE FREED! (if not NULL)
{
    id (*objects);
    @try {
        NSRange range = NSMakeRange(0, [anArray count]);
        objects = malloc(sizeof(id) * range.length);
        [anArray getObjects:objects range:range];
    }
    @catch (NSException * e) {
        // probably not an NSArray...
        // free(objects);  gives an error
        objects = NULL;
    }
    return objects;
}
