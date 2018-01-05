FROM ubuntu:16.04

ENV OS_FAMILY ubuntu
ENV OS_VERSION 16.04

RUN apt-get update && apt-get install -y \
    apt-transport-https \
    wget \
    curl \
    lsof \
    telnet \
    build-essential \
    git \
    openssl \
    libssl-dev \
    libncurses5 \
    libncurses5-dev \
    xsltproc \
    automake \
    autoconf

RUN curl -O https://raw.githubusercontent.com/kerl/kerl/master/kerl && mv kerl /usr/bin/kerl && chmod a+rwx /usr/bin/kerl

RUN rm -rf .kerl/builds/* && mkdir /erl19.3

RUN KERL_CONFIGURE_OPTIONS="--disable-hipe --enable-smp-support --enable-threads --enable-kernel-poll --without-odbc --with-ssl" kerl build 19.3 19.3 && kerl install 19.3 /erl19.3

ADD ./docker-entry.sh /docker-entry.sh

CMD ["/docker-entry.sh"]
