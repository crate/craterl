.PHONY: test clean

test:
	./priv/ci/run_tests.sh

clean:
	./rebar3 clean

doc: doc/edoc-info
	./rebar3 as doc edoc
