PLT=$(CURDIR)/.plt
DEPS=erts kernel stdlib

.PHONY: test xref dialyzer rebuild

all: compile test xref dialyzer

compile:
	@./rebar compile

clean:
	@- rm -rf $(CURDIR)/ebin
	@./rebar clean

test: compile
	@./rebar skip_deps=true eunit

qc: compile
	@./rebar qc

xref: compile
	@./rebar xref

clean_plt:
	- rm $(PLT)

$(PLT):
	@echo Building local plt at $(PLT)
	@echo
	@dialyzer --output_plt $(PLT) --build_plt \
	   --apps $(DEPS)

dialyzer: $(PLT)
	@dialyzer --fullpath --plt $(PLT) \
		-Wrace_conditions -Wunmatched_returns -Wno_return\
		-r ./ebin

rebuild: clean clean_plt all

##
## Doc targets
##

docs: compile
	./rebar skip_deps=true doc

pages: docs
	rm -rf /tmp/reiddraper-fn-docs
	mkdir -p /tmp/reiddraper-fn-docs
	cp -R doc/ /tmp/reiddraper-fn-docs
	git checkout gh-pages
	cp /tmp/reiddraper-fn-docs/* .
	git add .
	git add -u
	git commit
	git push origin gh-pages
	git checkout master
