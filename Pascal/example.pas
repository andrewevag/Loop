program hello;

var
	x, y : integer;
	z : array[0...10] of integer;
    w : integer;
begin
	x := 12;
	y := x div 2;
	z[x-y] := x + y * 4;
	for w := 1 to 12*12 do
	begin
		x := x * 2;
	end;
	if x > 3000 then
		x := 0;
end.
