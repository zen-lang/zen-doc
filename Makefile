.EXPORT_ALL_VARIABLES:
.PHONY: test

SHELL = bash

repl:
	clj -A:test:nrepl -m nrepl.cmdline

test:
	clj -A:test:kaocha

deploy:
	cd build && git init && git add . && git commit -m "first commit" & git branch -M master & git remote add origin git@github.com:zen-lang/zen-lang.github.io.git && git push -u --force origin master

pom:
	clj -X:deps mvn-pom

jar:
	clojure -X:depstar jar :jar target/zen-doc.jar

uberjar:
	clojure -X:depstar uberjar :jar target/zen-doc.uber.jar

pub:
	clj -A:deploy
