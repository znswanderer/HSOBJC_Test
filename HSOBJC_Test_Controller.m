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

void freeStablePtr(HsStablePtr aStablePointer);

HsStablePtr newStableIdContainer(id someObject);
id retrieveId(HsStablePtr aStablePointer);

id uppercase(id inputString);
id integerTest(id aNumber);
id doubleTest(id aNumber);
id countAllStrings(id x);

HsStablePtr newStoredArray(id someObject);
id retrieveStoredArray(HsStablePtr aStablePointer);


@implementation HSOBJC_Test_Controller

// StableId test

- (IBAction)storeStableId:(id)sender;
{
    DummyNSString *dummyString = [[DummyNSString alloc] init];
    [dummyString setStringValue:[stableId_inputTextField stringValue]];
    
    stableId_stableIdContainer = newStableIdContainer(dummyString);
    [dummyString release];
    
    [stableId_retrieveButton setEnabled:YES];
    [stableId_inputTextField setEnabled:NO];
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


// NSString test

- (IBAction)convertString:(id)sender;
{
    NSString *upperCaseString = uppercase([string_inputTextField stringValue]);
    [string_outputTextField setStringValue:upperCaseString];
}

// NSNumber test
- (IBAction)newNumberFromSlider:(id)sender;
{
    NSNumber *value = [NSNumber numberWithDouble:[(NSSlider*)sender doubleValue]];

    // double
    NSNumber *doubleRes = doubleTest(value);
    [number_doubleValue setDoubleValue:[doubleRes doubleValue]];
    
    // integer
    NSNumber *intRes = integerTest(value);
    [number_integerValue setIntValue:[intRes intValue]];
    
}

// NSArray test
- (IBAction)arrayInput:(id)sender;
{
    NSArray *inputArray = [[(NSTextField*)sender stringValue] componentsSeparatedByString:@", "];
    
    NSArray *results = countAllStrings(inputArray);
    [array_stringResults setStringValue:[results description]];
}

// Stored Array test
- (IBAction)storeArray:(id)sender;
{
    NSArray *inputArray = [[(NSTextField*)sender stringValue] componentsSeparatedByString:@", "];
    
    storedArray = newStoredArray(inputArray);
    
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

    freeStablePtr(storedArray);
    storedArray = NULL;
}

// Error test
- (IBAction)constellation:(id)sender;
{
    NSArray *anArray = [NSArray array];
    uppercase(anArray);
}

@end