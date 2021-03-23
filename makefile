all: *.erl
	# copy tq application
	#cp /home/sanya/QT/build-erl_calc-Desktop-Debug/erl_calc .
	erlc *.erl
	./emake.sh