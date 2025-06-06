FROM ubuntu:24.04

ENV PATH /usr/local/bin:$PATH

# 设置环境变量以避免交互安装，并配置时区
ENV DEBIAN_FRONTEND=noninteractive \
    TZ=Asia/Shanghai \
    LANG=C.UTF-8

# 配置ubuntu-releases源为阿里云镜像 
# 更新软件包列表并安装必要的软件包
# https://developer.aliyun.com/mirror/ubuntu-releases/
RUN sed -i "s@http://.*archive.ubuntu.com@https://mirrors.aliyun.com/@g" /etc/apt/sources.list \
    && sed -i "s@http://.*security.ubuntu.com@https://mirrors.aliyun.com/@g" /etc/apt/sources.list \
    && apt update -y; \
    apt install -y --no-install-recommends \
    tzdata vim openssh-client git curl wget aria2 telnet netcat-openbsd net-tools inetutils-ping dnsutils procps gcc make m4 libncurses-dev;\ 
	rm -rf /var/lib/apt/lists/*
# libx11-dev; # TODO: 编译带libx11-dev的mit-scheme似乎有问题
# 设置时区为上海
RUN ln -sf /usr/share/zoneinfo/${TZ} /etc/localtime && \
    echo "${TZ}" >/etc/timezone

ENV SCMUTILS_VERSION 12.1
ENV SCHEME_VERSION 12.1
ENV SCHEME_MD5 45a83d61104f9e41c542a9732cea4e3c
ENV ARCH x86-64

# export SCHEME_VERSION=12.1
# export SCHEME_MD5=45a83d61104f9e41c542a9732cea4e3c
# export ARCH=x86-64

# 直接拷贝本地文件
# COPY mit-scheme-${SCHEME_VERSION}-${ARCH}.tar.gz .

# REF：https://www.gnu.org/software/mit-scheme/documentation/stable/mit-scheme-user.html
RUN set -eux; \
    wget --no-check-certificate -O mit-scheme-${SCHEME_VERSION}-${ARCH}.tar.gz "https://ftp.gnu.org/gnu/mit-scheme/stable.pkg/${SCHEME_VERSION}/mit-scheme-${SCHEME_VERSION}-${ARCH}.tar.gz"; \
    echo "${SCHEME_MD5}  mit-scheme-${SCHEME_VERSION}-${ARCH}.tar.gz" | md5sum -c -; \
    tar -xvzf mit-scheme-${SCHEME_VERSION}-${ARCH}.tar.gz; \
    rm mit-scheme-${SCHEME_VERSION}-${ARCH}.tar.gz; \
    cd mit-scheme-${SCHEME_VERSION}*/src; \
    ./configure; \
    nproc="$(nproc)"; \
    make -j "$nproc"; \
    make install; \
    cd ../..; \
    rm -rf mit-scheme-${SCHEME_VERSION}*;\
    mit-scheme --eval "(begin (identify-world)(exit))" 

RUN set -eux; \
    wget --no-check-certificate -O scmutils-${SCMUTILS_VERSION}.tar.gz  https://groups.csail.mit.edu/mac/users/gjs/6946/mechanics-system-installation/native-code/scmutils-20230902.tar.gz; \
    tar -xvzf scmutils-${SCMUTILS_VERSION}.tar.gz; \
    rm scmutils-${SCMUTILS_VERSION}.tar.gz; \
    cd scmutils-${SCMUTILS_VERSION}; \
    ./install.sh; \
    mv mechanics.sh ../; \
    mv uninstall.sh ../; \
    cd ..; \
    rm -rf scmutils-${SCMUTILS_VERSION}; \
    ./mechanics.sh --eval "(begin (display \"enjoy\!\")(exit))"

CMD ["/bin/bash", "./mechanics.sh"]
