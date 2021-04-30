all: build run

build: *.erl
	# copy tq application
	# cp /home/sanya/source/build-etoplan_inerfake-Desktop-Debug/etoplan_inerfake erl_calc
	erlc *.erl

run:
	./emake.sh
