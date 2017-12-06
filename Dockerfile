FROM erlang:20.1.6-alpine
MAINTAINER Pierre Fenoll <pierrefenoll@gmail.com>

WORKDIR /usr/src/app
COPY . $PWD

RUN set -x \
 && apk update && apk upgrade \
 && apk add curl make \
 && rm -r /var/cache/apk/* \
 && curl --fail --output /usr/local/bin/rebar3 https://s3.amazonaws.com/rebar3/rebar3 \
 && chmod +x /usr/local/bin/rebar3 \
 && rebar3 --version \
 && rebar3 as prod release \
 && mkdir -p /opt/rel \
 && cp -r $PWD/_build/prod/rel/sample /opt/rel \
 && rm -r /usr/src/app

WORKDIR /opt/rel
ENV RELX_REPLACE_OS_VARS true
EXPOSE 6773

ENTRYPOINT ["/opt/rel/sample/bin/sample"]
CMD ["foreground"]
