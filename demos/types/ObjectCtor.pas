unit ObjectCtorDemo;

implementation

uses
    System;

type
    SomeObject = record
        field1: Integer;
        field2: Boolean;
    end;

initialization

// named
var someA := SomeObject(field1: 123; field2: true);

WriteLn('someA: ' + someA.field1 +', ' + someA.field2);

// inferred
var someB: SomeObject := (field1: 456; field2: false);

WriteLn('someB: ' + someB.field1 +', ' + someB.field2);

end.