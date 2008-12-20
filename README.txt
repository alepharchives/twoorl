To run Twoorl follow the following steps:

PRE-REQUIREMENTS:
- Get the latest version of ErlyWeb (prior to 0.7.1, this would be from trunk) and Yaws
- Install MySQL and create a MySQL database for twoorl.
- Run twoorl.sql to create the Twoorl tables.


BOOT OPTION A - standalone yaws server
	1. Edit src/twoorl_app.hrl with your appropriate environment variables
	2. Open a shell and type the following in twoorl's directory:

		make run
		
	This should execute the following steps:
		- Compile the files defined in the Emakefile file using make:all()
			- src/twoorl_util.erl
			- src/twoorl.erl
		- Start Yaws and then execute "twoorl:start()" (using the info in yaws.conf)


BOOT OPTION B - as part of an OTP supervisor tree (with yaws in embedded mode)
	(Notice: this boot option isn't working - YET - on the 'packaged' branch)
	1. Edit ebin/twoorl.app with your appropriate environment variables
	2. Open a shell and type the following in twoorl's directory:

		make run-app
	
	This should execute the following steps:
		- Compile the files defined in the Emakefile file using make:all()
			- src/twoorl_util.erl
			- src/twoorl.erl
		- start the following applications (in the given order):
			- application:start(inets)
			- application:start(crypto)
			- application:start(mnesia)
			- application:start(twoorl)
		  This last step will execute twoorl:start/2 not twoorl:start/1 (which
		  is the one used in the BOOT OPTION A.
	$ make clean && make

---
Additional notes for Git branch "packaged"
This branch includes the changes made in branch "non_root_install" and repacks
the project using erlang packages (http://www.erlang.org/doc/man/packages.html).
For now this code will only work with code from this erlyweb branch:
http://github.com/davide/erlyweb/tree/erlyweb-packaged
The main advantage theses changes bring is that you can now host several erlyweb
webapps independently in the same yaws server running (without code clashes).

Enjoy!
Davide :) (with many thanks to Yariv and the community for sharing their code!)
