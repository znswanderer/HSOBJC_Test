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
#import "DummyNSString.h"
#import "HSObjC_C.h"

id initController(NSDictionary *ivars);

void freeStablePtr(HsStablePtr aStablePointer);

HsStablePtr newStableIdContainer(id someObject);
id retrieveId(HsStablePtr aStablePointer);

HSValue *newStoredArray(id someObject);
id retrieveStoredArray(HSValue *aValue);

id getFunctionList(void);

@implementation HSOBJC_Test_Controller

- (void)awakeFromNib
{
    NSDictionary *ivars = [self ivarDictionary];
    NSLog(@"ivars: %@", ivars);
    
    HSValue *controller = initController(ivars);
    [controller retain];
    NSLog(@"controller: %@", controller);
    
    
    
    NSLog(@"registering functions");
    
    funcList = [getFunctionList() retain];
    NSLog(@"funclist: %@", funcList);

    // shortcuts
    doubleSqrt = [funcList objectForKey:@"doubleSqrt"];
    lengthOfStrings = [funcList objectForKey:@"lengthOfStrings"];
    squareInt = [funcList objectForKey:@"squareInt"];
    uppercase2 = [funcList objectForKey:@"uppercase2"];
        
     
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

// StableId test

- (IBAction)storeStableId:(id)sender;
{
    DummyNSString *dummyString = [[DummyNSString alloc] init];
    [dummyString setStringValue:[stableId_inputTextField stringValue]];
    
    stableId_stableIdContainer = newStableIdContainer(dummyString);
    [dummyString release];
    
    [stableId_retrieveButton setEnabled:YES];

    
    //[stableId_inputTextField setEnabled:NO];
    NSLog(@"valeForKey: %@", [stableId_inputTextField valueForKey:@"enabled"]);
    [stableId_inputTextField setValue:[NSNumber numberWithBool:NO] forKey:@"enabled"];
    NSLog(@"valeForKey: %@", [stableId_inputTextField valueForKey:@"enabled"]);
    
}

- (IBAction)retrieveStableId:(id)sender;
{
    DummyNSString *dummyString = (DummyNSString*)retrieveId(stableId_stableIdContainer);
    [stableID_outputTextField setStringValue:[dummyString stringValue]];
    
    [stableId_retrieveButton setEnabled:NO];
    [stableId_inputTextField setEnabled:YES];
    [stableId_inputTextField selectText:self];

    freeStablePtr(stableId_stableIdContainer);   
    stableId_stableIdContainer = NULL;
}




// NSArray test
- (IBAction)arrayInput:(id)sender;
{
    NSArray *inputArray = [[(NSTextField*)sender stringValue] componentsSeparatedByString:@", "];
    
    NSArray *results = [lengthOfStrings callWithArg:inputArray];
    [array_stringResults setStringValue:[results description]];
}

// Stored Array test
- (IBAction)storeArray:(id)sender;
{
    NSArray *inputArray = [[(NSTextField*)sender stringValue] componentsSeparatedByString:@", "];
    
    storedArray = newStoredArray(inputArray);
    [storedArray retain];
    
    [storeArray_retrieveButton setEnabled:YES];
    [storeArray_inputTextField setEnabled:NO];

}

- (IBAction)retrieveStoredArray:(id)sender;
{
    NSArray *result = (NSArray*)retrieveStoredArray(storedArray);
    [storeArray_stringResults setStringValue:[result description]];
        
    [storeArray_retrieveButton setEnabled:NO];
    [storeArray_inputTextField setEnabled:YES];
    [storeArray_inputTextField selectText:self];

    //freeStablePtr(storedArray);
    [storedArray release];
    storedArray = nil;
}

// Error test
- (IBAction)constellation:(id)sender;
{
    [uppercase2 callWithArg:nil];
}

@end
