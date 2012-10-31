.PHONY: test

all: compile

compile:
	@./rebar compile

clean:
	@./rebar clean

test: compile
	@./rebar skip_deps=true eunit

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
