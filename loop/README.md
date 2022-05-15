# Loop

Όλα τα προγράμματα έχουν γραφεί με την υπόθεση ότι input του προγράμματος είναι στις μεταβλητές i1, i2, ... και output στις μεταβλητές o1, o2, ... για να επιβεβαιωθεί η ορθότητα τους σε έναν διερμηνέα.

**Σχόλια** θεωρούνται γραμμές που ξεκινούν με "--"
## 1. 
### mult(x, y)

``` bash
y := i1;
z := i2;

x := 0;
for w := 1 to z do
	x := add(x, y);
done
o1 := x;
```

### sub(x, y)
``` bash
x := i1;
y := i2;
for w := 1 to y do
	x := x - 1;
done
o1 := x;
```

### ifzero(x, y)
``` bash
x := i1;
y := 1;
for w := 1 to x do
	y := y - 1;
done
o1 := y;
```

## 2.
### if then else
``` bash
z := g(x);
y := ifzero(z);
for w := 1 to y do
	h1(x);
done

y1 := ifnzero(z);
for w := 1 to y1 do
	h2(x);
done
```

## 3.
### div(x, y)
``` bash
x := i1;
y := i2;

-- if divisor = 0 => output = 0
z := ifzero(y);
for w := 1 to z do
	o1 := 0;
done

z := ifnzero(y);
r := 0;
-- if divisor != 0 => division
for w := 1 to z do
	x1 := x;
	
	for i := 1 to x do
		
		x2 := greater(x1, y);
		x3 := equals(x1, y);
		x2 := or(x2, x3);
-- if >= subtract && increment counter r
		for j := 1 to x2 do
			x1 := sub(x1, y); 
			r := r + 1;
		done

	done
	o1 := r;
done
```

## 4.
### $x^y$ - pow
``` bash
x := i1;
y := i2;

x1 := 1;
for w := 1 to y do
	x1 := mult(x1, x);
done
o1 := x1;

-- this can be faster by x <- x * x;
```

### $n!$
``` bash
x := i1;
z := 1;
for w := 1 to x do
	z := mult(z, w);
done

o1 := z;
```

### $\binom{n}{a}$
``` bash
n := i1;
k := i2;
n1 := factorial(n);
k1 := factorial(k);
d := sub(n, k);
d := factorial(d);
d := mult(d, k1);
o1 := div(n1, d);
```

### $\lfloor \sqrt{n} \rfloor$
``` bash
x := i1;
-- k := 2;
k := 1;
k := k + 1;
x := div(x, k);
z := 0;
for w := 1 to x do
	k1 := mult(w, w);

	k2 := equals(k1, x);
	for i := 1 to k2 do
		z := w;
	done

	k := ifzero(z);
	k2 := greater(k1, x);
	k := and(k2, k);
	for i := 1 to k do 
		z := w -1 ;
	done

done
o1 := z;
```