
all:
	alex Lexer.x
	happy Parser.y
	ghc --make Main -o simplebool

#test:
#	echo fork leaf leaf | ./foo
#	echo fork leaf leafqleaf | ./foo
#	echo leaf leaf leaf leaf leaf | ./foo

clean:
	rm -f *.o *.hi Parser.hs Lexer.hs simplebool

