ARG deployment
FROM registry.gitlab.com/riak/riak/base/${deployment}:latest
ARG deployment
ADD . /var/src/riak/
RUN sh -x ./build.sh
