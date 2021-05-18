Version = erltoplan-0.0.1

build: *.erl
	# copy tq application
	# cp /home/sanya/source/build-etoplan_inerfake-Desktop-Debug/etoplan_inerfake erl_calc
	erlc *.erl

install: build
	# add files to erlang libs
	mkdir /lib/erlang/lib/$(Version)
	mkdir /lib/erlang/lib/$(Version)/ebin
	mv *.beam /usr/lib/erlang/lib/$(Version)/ebin
	# made emake.sh globally allowed
	cp emake.sh /bin

remove:
	rm -r /lib/erlang/lib/$(Version)
	rm /bin/emake.sh

clean:
	rm *.beam

run:
	./emake.sh -h
