all:
	erlc -o beam parser/*.erl gui/*.erl
	./beam/emake.sh