REBAR=./rebar
CODE_DIR=apps/calculator
CT_LOG_DIR=apps/calculator/ctest/*/logs
NODE?=calculator
CONFIG?=rel/files/sys.config
SERVER := erl -pa apps/calculator/ebin -pa deps/*/ebin -smp enable -setcookie POPCORN -config ${CONFIG} ${ERL_ARGS}

all:
	${REBAR} compile

gc:
	@echo 'Removing all emacs backup files'
	@find . -name "*~" -exec rm -f {} \;
	@find . -name "erl_crash.dump" -exec rm -f {} \;
	@rm -f ${CODE_DIR}/src/*.P
	@rm -f ${CODE_DIR}/src/*/*.P
	@rm -f ${CODE_DIR}/src/*.beam
	@rm -f ${CODE_DIR}/src/*/*.beam
	@echo 'Removing all common_test logs'
	@rm -rf ${CT_LOG_DIR}/*.*
	@rm -f ${CT_LOG_DIR}/variables-ct*

test:
	@make eunit && make ct

eunit: all
	ERL_LIBS=deps ${REBAR} eunit skip_deps=true

## To get the ptl file use
##    dialyzer --build_plt --apps erts kernel stdlib
dialyze: all
	dialyzer --verbose --plt .dialyzer-R15B01.plt \
		-Wunmatched_returns -Werror_handling \
		apps/*/ebin

dialyze_plt: all
	dialyzer --verbose --build_plt \
	  --output_plt .dialyzer-R15B01.plt \
		--apps kernel stdlib sasl erts ssl \
		  tools os_mon runtime_tools crypto \
			inets xmerl webtool eunit syntax_tools \
			compiler edoc hipe mnesia otp_mibs public_key \
			snmp -pa apps/*/ebin

types:
	typer --plt .dialyzer-R15B01.plt -r apps/calculator/src -I apps/calculator/include -I apps/calculator -pa deps/*/ebin --annotate

xref:
	${REBAR} skip_deps=true xref

shell:
		${SERVER} -name ${NODE}@`hostname` -boot start_sasl -s crypto -s lager -s calculator
