program NamesChecker;

    //fills the histogram with the frequency of letters
    procedure fillHist(var name:string;var hist:array of integer);
    var i:integer;
    var len:integer;
    var first:char;
    begin
        len:=Length(name);
        for first:='a' to 'z' do
            hist[ord(first)]:=0;
        for i:=1 to len do
            hist[ord(name[i])]:=hist[ord(name[i])]+1;
    end;

    //checks akk the conditions
    procedure isLettersOkay(var hist1:array of integer;var hist2:array of integer;var histProposed:array of integer);
    var letterInOne:boolean;
    var letterInTwo:boolean;
    var letterNotInBoth:boolean;
    var letterInBoth:boolean;
    var first:char;
    var inName1:boolean;
    var inName2:boolean;
    begin
        letterInOne:=false;
        letterInTwo:=false;
        letterInBoth:=false;
        letterNotInBoth:=false;
        for first:='a' to 'z' do
        begin
            if histProposed[ord(first)]>0 then
            begin
                inName1:=hist1[ord(first)]>0;
                inName2:=hist2[ord(first)]>0;
                if inName1 and inName2 then //fails condition of letter in both words
                begin
                    letterInOne:=true;
                    letterInTwo:=true;
                    letterInBoth:=true;
                end;
                if inName1 and not inName2 then //satisfy condition 1
                    letterInOne:=true;
                if not inName1 and  inName2 then //satisfy condition 1
                    letterInTwo:=true;
                if not inName1 and not inName2 then //fails condition of letter that does not exist
                    letterNotInBoth:=true;
            end;
        end;
        if letterInOne and letterInTwo and (not letterInBoth) and (not letterNotInBoth) then
            writeln('TRUE')
        else
            writeln('FALSE');
    end;

    //print the frequency of used letters
    procedure printAllHist(var hist1:array of integer;var hist2:array of integer;var histProposed:array of integer);
    var first:char;
    begin
        for first:='a' to 'z' do
        begin
            if (hist1[ord(first)]+hist2[ord(first)]+histProposed[ord(first)])>0 then
            begin
                write(first);
                write(' ');
                writeln(hist1[ord(first)]+hist2[ord(first)]+histProposed[ord(first)]);
            end;
        end;
    end;

var name1:string;
var name2:string;
var proposedName:string;
var histName1:array ['a'..'z'] of integer;
var histName2:array ['a'..'z'] of integer;
var histProposedName:array ['a'..'z'] of integer;
begin
    ReadLn(name1);
    ReadLn(name2);
    ReadLn(proposedName);
    fillHist(name1,histName1);
    fillHist(name2,histName2);
    fillHist(proposedName,histProposedName);
    isLettersOkay(histName1,histName2,histProposedName);
    printAllHist(histName1,histName2,histProposedName);
end.