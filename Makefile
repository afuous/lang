run:
	stack build && stack exec lang test.txt > out.c && gcc out.c && ./a.out
