REBAR3 ?= rebar3

all: fmt
	$(REBAR3) as prod release -o artifacts

start-service:
	./artifacts/sample/bin/sample foreground

image: fmt
	docker build -t build__sample --file=alpine/build/Dockerfile .
	docker run --rm --volume="$$PWD"/alpine/run/artifacts:/artifacts:rw build__sample
	docker build -t run__sample alpine/run/
#	docker run --rm -it -p 6773:6773 run__sample

fmt:
	./bin/format-json.sh $(wildcard priv/*.json)
