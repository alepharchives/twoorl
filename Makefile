all: code

code: clean
	erl -noshell -eval 'filelib:ensure_dir("./ebin/"), filelib:ensure_dir("./ebin/twoorl/"), make:all().' -pa ebin -s erlang halt

run:	code
	erl -noshell -eval 'filelib:ensure_dir("./log/").' -pa ebin -s erlang halt
	erl -run yaws -yaws debug -conf yaws.conf

run-app:	code
	erl -sname twoorlapp -setcookie twoorl -mnesia dir "'twoorl.mnesia'" -yaws embedded true -pa ebin -boot start_sasl -eval '[application:start(X) || X <- [inets, crypto, mnesia, twoorl]]'
	
clean:
	rm -fv ebin/*.beam ebin/twoorl/*.beam twoorl.rel twoorl.script twoorl.boot erl_crash.dump *.log *.access

cleandb:
	rm -rfv *.mnesia Mnesia*

cleandocs:
	rm -fv doc/*.html
	rm -fv doc/edoc-info
	rm -fv doc/*.css
	rm -fv doc/*.png