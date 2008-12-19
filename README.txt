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

---
Additional notes for Git branch "non_root_install"
This branch implements example code of deploying twoorl on a path different from
the default root. There are many components that must work together for such an
installation to be successful:
 - erlyweb must support it (my fork does: http://github.com/davide/erlyweb/tree/non_root_install)
 - the view must get #arg as the first argument and use it to correctly prefix their
   links via erlyweb:get_app_root(A).
 - the javascript code must get hold of the app_root in order to build correct links
   (currently this is done using an object (Twoorl) in the global namespace)

Enjoy!
Davide :) (with many thanks to Yariv and the community for sharing their code!)