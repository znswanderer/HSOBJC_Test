//
//  DummyNSString.m
//  HSOBJC_Test
//
//  Created by Arbeit on 09.02.10.
//  Copyright 2010 __MyCompanyName__. All rights reserved.
//

#import "DummyNSString.h"


@implementation DummyNSString

- (void) dealloc
{
    NSLog(@"dealloc DummyNSString: %@", [self stringValue]);
    [self setStringValue:nil];
    [super dealloc];
}

- (NSString *)stringValue {
    return [[stringValue retain] autorelease];
}

- (void)setStringValue:(NSString *)value {
    if (stringValue != value) {
        [stringValue release];
        stringValue = [value copy];
    }
}


@end
