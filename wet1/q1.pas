program PascalTriangle;

    //function to print the line
    procedure printLine(size:integer;line:array of integer);
        var j:integer;
        begin
            write(line[0]);
            if size>1 then
            begin
                for j:=1 to size-1 do
                begin
                    write(' ');
                    write(line[j]);
                end;
            end;
        end;

var n: integer;
var line: array of integer;
var i:integer;
var j:integer;
var prev:integer;
var temp:integer;

begin
ReadLn(n);
SetLength(line,n);
for i :=0 to n-1 do
    line[i]:=1;
for i := 1 to n do
    begin
            //calculate next line
            begin
                if i>2 then
                begin
                    prev:=line[0];
                    for j :=1 to i-2 do
                    begin
                        temp:=line[j];
                        line[j]:=prev+temp;
                        prev:=temp;
                    end;
                end;
            end;

            printLine(i,line);
            writeln();
    end
end.