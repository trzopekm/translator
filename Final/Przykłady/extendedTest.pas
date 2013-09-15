program extendedTest(input, output);        
var
        i, j, k, n:integer;
		a,b :real;

procedure Initiate(a,b:real);
begin
		i := 5 + int(a);
		randomize;
        j := random(10);
		k := sin(cos(20));
		inc(k);
		dec(j);
		writeln(i);
		writeln(j + k);
		readln(n);
end;

function Sum(a,b:integer): integer;
begin
	if n = 1 then
		begin
          	a := 5;
   			i := 5+b;
			Sum := a + b - i;
		end
        else
		begin
            a := exp(5);
   			i := 5+ln(a);
		end;
		
for counter := 1 to 5 do
  begin
   a := a+1;
   i := counter - 1;
  end;

repeat
begin
   a := 5;
   i := 5+a;
   k := Sum(i,j) * 2;
        
end
until (S=N);
        Sum := a + b;
end;

begin

Initiate(2.4,1.53);

if n = 1 then
		begin
          	a := 5;
			i := 5+a;
			if n = 1 then
				begin
					a := Sum(a,i);
					i := 5+a;
				end
			else
				begin
					a := 5;
					i := 5+a;
				end;
		end
        else
		begin
            a := 5;
   			i := 5+a;
		end;

end.