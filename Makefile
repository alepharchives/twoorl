all: code

code: clean
	erl -s make all load -s init stop

run:	code
	erl -noshell -eval 'filelib:ensure_dir("./log/").' -pa ebin -s erlang halt
	erl -yaws debug -run yaws --conf yaws.conf

run-supervised:	code
	erl -sname twoorlapp -setcookie twoorl -mnesia dir "'twoorl.mnesia'" -yaws embedded true -pa ebin -boot start_sasl -eval '[application:start(X) || X <- [inets, crypto, mnesia, twoorl]]'
	
clean:
	rm -fv ebin/*.beam twoorl.rel twoorl.script twoorl.boot erl_crash.dump *.log *.access

cleandb:
	rm -rfv *.mnesia Mnesia*

cleandocs:
	rm -fv doc/*.html
	rm -fv doc/edoc-info
	rm -fv doc/*.css
	rm -fv doc/*.png