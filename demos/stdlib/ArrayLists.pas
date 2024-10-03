implementation
uses 
    System,
    System.Collections;
    
type WordObject = class
    word: String;
end;
   
function PrintIntegers(list: ArrayList[Integer]);
begin
    for var i := 0 to list.Length() - 1 do
        Write(list.Get(i).ToString());
    WriteLn('');
end;

function PrintObjects(list: ArrayList[WordObject]);
begin
    for var i := 0 to list.Length() - 1 do
        Write(list.Get(i).word);
    WriteLn('');
end;

initialization
    var ints := NewArrayList[Integer]();
    
    ints.Add(1);
    ints.Add(2);
    ints.Add(3);
    
    PrintIntegers(ints);
    
    ints.Remove(1);
    
    PrintIntegers(ints);

    var words := NewArrayList[WordObject]();
    words.Add(WordObject(word: 'Hello, '));
    words.Add(WordObject(word: 'world!'));
    
    PrintObjects(words);
    
    words.Remove(words.Length - 1);
    words.Add(WordObject(word: 'Alice'));
    
    PrintObjects(words);
end.
