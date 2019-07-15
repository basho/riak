FROM centos:7
# https://gist.github.com/seanjensengrey/808544636b5f3715625584fc1c97615a
RUN curl -O https://raw.githubusercontent.com/kerl/kerl/master/kerl
RUN chmod +x kerl
RUN yum install -y gcc gcc-c++ glibc-devel make ncurses-devel openssl-devel \
	autoconf java-1.8.0-openjdk-devel git \
	pam-devel perl-Digest-SHA
RUN CFLAGS="-DOPENSSL_NO_EC=1" ./kerl build git https://github.com/basho/otp OTP_R16B02_basho10 R16B02-basho10
RUN ./kerl install R16B02-basho10 /opt/erlang/R16B02-basho10
RUN . /opt/erlang/R16B02-basho10/activate
RUN mkdir -p /var/src/riak
WORKDIR /var/src/riak
RUN yum install -y which
RUN yum install -y rpm-build