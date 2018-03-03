REBAR = ./rebar3

BUILD_DIR = _build

all:
	$(REBAR) compile
	$(REBAR) unlock

shell:
	$(REBAR) shell
	$(REBAR) unlock

check:
	$(REBAR) eunit

clean:
	$(REBAR) clean -a
	$(REBAR) unlock

distclean: clean
	rm -rf $(BUILD_DIR)

version:
	@erl \
		-noshell \
		-eval 'io:format("~s~n", [\
			proplists:get_value(vsn, \
			element(3, \
			hd(\
			element(2, \
			file:consult("src/cpflib.app.src")))))\
		])' \
		-s init stop
