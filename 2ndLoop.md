# Κωδικοποίηση
## 1.
### C - encode(m, n)
``` bash
m := i1;
n := i2;
-- x := (n + m)*(n + m + 1)/2;
x1 := add(m, n);
x2 := x1 + 1;
x := mult(x1, x2);

-- y := 2;
y := 1;
y := y + 1;

x := div(x, y);
o1 := add(x, n);
```

### $D_1$ - decode1(z)
``` bash
-- for D1 we need n = z - f^(-1)(floor(f(z)))
-- f(z) = (sqrt(1 + 8z) - 1) / 2
-- f^(-1)(z) = (z^2 + z) / 2
z := i1;
t := z;
x := 1;
x := x + 1;
x := add(x, x);
x := add(x, x);
z := mult(z, x);
z := z + 1;
z := sqrt(z);
z := z - 1;
x := 1;
x := x + 1;
-- z := z div 2;
z := div(z, x);
w := mult(z, z);
z := add(z, w);
z := div(z, x);
o1 := sub(t, z);
```

### $D_2$ - decode2(z)
``` bash
-- D2 m = floor(f(z)) - decode1
-- f(z) = (sqrt(1+8z) -1 )/2
z := i1;
z1 := z;
t := z;
x := 1;
x := x + 1;
x := add(x, x);
x := add(x, x);
z := mult(z, x);
z := z + 1;
z := sqrt(z);
z := z - 1;
x := 1;
x := x + 1;
-- z := z div 2;
z := div(z, x);
z1 := decode1(z1);
o1 := sub(z, z1);
```
<div style="page-break-after: always;"></div>

## 2.
### (α)
Έχουμε για μικρές τιμές του $n$ τις ακόλουθες προϋποθέσεις ώστε να συνάδει το αποτέλεσμα με τις συνήθεις πράξεις.:

$f(x, y, 0) = S(y)$

$f(x, y, 1) = 
 \begin{cases} 
      f(x, y-1, 1) + 1 & y \ne 0 \\
      x & y = 0
   \end{cases}
$

$f(x, y, 2) = 
 \begin{cases} 
      f(x, y-1, 2) + x & y \ne 0 \\
      0 & y = 0
   \end{cases}
$

$f(x, y, 3) = 
 \begin{cases} 
      f(x, y-1, 3) * x & y \ne 0 \\
      1 & y = 0
   \end{cases}
$

$f(x, y, 4) = 
 \begin{cases} 
      (f(x, y-1, 4))^x & y \ne 0 \\
      x & y = 0
   \end{cases}
$

Επομένως ο κλειστός τύπος είναι :

$
f(x, y, 0) = y + 1 \\
f(x, 0, 1) = x \\
f(x, 0, 2) = 0 \\
f(x, 0, 3) = 1 \\
f(x, 0, n) = x, n > 3 \\
f(x, y, n) = f(x, f(x, y-1, n), n-1), y>0, n > 0
$

### (β)
Συνάρτηση που την υπολογίζει στην Pascal
``` Pascal
function f(x, y, n: integer) : integer;
begin
	if n = 0 then
		f := y + 1;
	else 
		if y = 0 then
			case n of
				1 : f := x;
				2 : f := 0;
				3 : f := 1;
			else f := x;
			end;
		else
			f := f(x, f(x, y-1, n), n-1);
end;
```
Στην loop αυτή η συνάρτηση δεν μπορεί να υπολογιστεί γιατί δεν είναι πρωταρχικά αναδρομική.
