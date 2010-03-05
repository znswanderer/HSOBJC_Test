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

#import "HSOBJC_Test_Controller.h"
#import "HSObjC_C.h"


@implementation HSOBJC_Test_Controller

- (void)awakeFromNib
{
    controller = initController([self ivarDictionary]);
    [controller retain];
}

- (NSDictionary*)ivarDictionary;
{
    NSMutableDictionary *ivarDict = [[NSMutableDictionary alloc] init];

    unsigned int outCount;
    Ivar *list = class_copyIvarList([self class], &outCount);
    for (unsigned int i=0; i<outCount; i++) {
        const char *prop = ivar_getName(list[i]);
        id value = object_getIvar(self, list[i]);
        if (value) {
            [ivarDict setObject:value forKey:[NSString stringWithCString:prop encoding:NSASCIIStringEncoding]];
        }
    }
    
    free(list);
    return [ivarDict autorelease];
}


// The following two IBActions could be implemented in the Controller.hs like
// all the other actions. But I just wanted to keep two "original" Objective-C
// actions to demonstrate the calling of Haskell controller "methods"
- (IBAction)arrayInput:(id)sender;
{
    NSArray *inputArray = [[(NSTextField*)sender stringValue] componentsSeparatedByString:@", "];
    
    NSArray *results = [getMethod(controller, @"lengthOfStrings") callWithArg:inputArray];
    [array_stringResults setStringValue:[results description]];
}


// Error test
- (IBAction)constellation:(id)sender;
{
    [getMethod(controller, @"lengthOfStrings") callWithArg:[NSArray arrayWithObject:[NSNumber numberWithInt:0]]];
}

@end
