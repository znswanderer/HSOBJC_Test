//
//  DummyNSString.h
//  HSOBJC_Test
//
//  Created by Arbeit on 09.02.10.
//  Copyright 2010 __MyCompanyName__. All rights reserved.
//

#import <Cocoa/Cocoa.h>


@interface DummyNSString : NSObject {
    NSString *stringValue;
}
- (NSString *)stringValue;
- (void)setStringValue:(NSString *)value;


@end
