program hello

var
	x, y : integer;
	z : array[0...10] of integer

begin
	x := 12;
	y := x div 2;
	z[x-y] := x + y * 4
end.
