# Pascal Reduced Syntax for translating to loop

```
	<expr> ::= <simple expression> | <simple expression> <relational operation> <simple expression>
	
	<simple expression> ::= <term> | <sign> <term> | <simple expression> <adding operator> <term>
	
	<relational operator> ::= = | <> | < | > | <= | >=
	<adding operator> ::= + | - 
	<sign> ::= + | -

	<term> ::= <factor> | <term> <multiplying operator> <factor>

	<multiplying operator> ::= * | / | div | mod

	<factor> ::= <variable> | <unsigned constant> | ( <expression> )
	<unsigned constant> ::= <digit> <unsigned constant> | ε
	
	<digit> ::= 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9

	<statement> ::= <assignment statement> | <structured statement>
	
	<assignment statement> ::= <variable> := <expression>
	
	<variable> ::= <identifier> | <identifer> [ <expression> ]
	
	<structured statement> ::= <compound statement> | <if statement> | <case element> | <for statement>

	<compound statement> ::= begin <statement> {; <statement>} end;

	<if statement> ::= if <expr> then <statement> | if <expr> then <statement> else <statement>

	<case element> ::= case <expression> of <case list elem> end

	<case list elem> ::= <unsigned constant> : <statement> <case list elem> | ε


	<for statement> ::= for <identifier> := <expression> to <expression> do <statement>

	<program> ::= program <identifier> <var definitions> begin <statement> {; <statement>} end.

	<var definitions> ::= var <variable declaration> {; <variable declaration>} | ε

	<var definition> ::= <identifier> {, <identifier>} : <type>
	<type> ::= integer | array[<range>] of <integer>
	<range> ::= <unsigned constant> ... <unsigned constant>

```

# H διαδικασία
Θα δείξουμε πως γίνεται η μετατροπή από την περιρισμένη Pascal σε Loop προγράμματα.

Η ισοδυναμία των δύο βασίζεται στο γεγονός
ότι ένα **expression** μπορεί να υπολογιστεί διατρέχοντας το συντακτικό του δέντρο και αποθηκεύοντας προσωρινά δεδομένα σε ξεχωριστές μεταβλητές. Τα **if statements** μπορούν να γίνουν ισοδύναμα στην loop θεωρώντας τις τιμές bool ως αριθμούς στο {0, 1}.
Τα **case statements** μπορούν επίσης να μοντελοποιηθούν μεσω διαδοχικών **if statements** και άρα δεν διαφέρουν από τα **if statements**. Τέλος το **for statement** είναι ισοδύναμο στην loop αν βρεθεί το εύρος στο οποίο θα γίνει η επανάληψη (δηλαδή το κάτω και το πάνω όριο αποτελούν **expressions**), τοποθετηθεί το κάτω εύρος σε μία μεταβλητή και κάθε φορά προστίθεται σε αυτήν το *loop variable* ώστε να έχουμε αυτή ως τον πραγματικό *loop counter*.

Στην μετατροπή δεν θα χρησιμοποιηθούν αρνητικοί αριθμοί παρότι αυτοί μπορούν να κωδικοποιηθούν από τους θετικούς ή να χρησιμοποηθεί για κάθε xi μία μεταβλητή xip με τιμές στο {0, 1} που καθορίζει το πρόσημο της xi.
Επιπλέον στην μετατροπή θα χρησιμοποιηθούν identifier strings για τις μεταβλητές της loop. Αυτό δεν έχει διαφορά με την χρήση μόνο xi αφού όλες οι identifier strings μπορούν και αυτές να κωδικοποιηθούν από φυσικούς αριθμούς.
