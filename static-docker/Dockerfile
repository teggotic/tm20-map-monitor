################################################################################
# Set up environment variables, OS packages, and scripts that are common to the
# build and distribution layers in this Dockerfile
FROM alpine:3.19 AS base

ENV GHC_VERSION=9.10.2

ENV GHCUP_INSTALL_BASE_PREFIX=/
ENV PATH=/.ghcup/bin:$PATH

ENV GHCUP_VERSION=0.1.50.2

# Install the basic required dependencies to run 'ghcup' and 'stack'
RUN apk upgrade --no-cache &&\
    apk add --no-cache \
        curl \
        gcc \
        git \
        libc-dev \
        xz \
        gmp-dev

# Download, verify, and install ghcup
RUN echo "Downloading and installing ghcup" &&\
    cd /tmp &&\
    wget -O /tmp/ghcup "https://downloads.haskell.org/~ghcup/${GHCUP_VERSION}/x86_64-linux-ghcup-${GHCUP_VERSION}" &&\
    # if ! echo -n "${GHCUP_SHA256}" | sha256sum -c -; then \
    #     exit 1 ;\
    # fi ;\
    mv /tmp/ghcup /usr/bin/ghcup &&\
    chmod +x /usr/bin/ghcup


################################################################################
# Intermediate layer that builds GHC
FROM base AS build-ghc

ARG GHC_VERSION=9.10.2

RUN echo "Install OS packages necessary to build GHC" &&\
    apk add --no-cache binutils-gold curl gcc g++ gmp-dev libc-dev libffi-dev make musl-dev ncurses-dev perl tar xz

ARG GHC_VERSION=9.10.2

RUN echo "Compiling and installing GHC" &&\
    ghcup install ghc ${GHC_VERSION}

FROM build-ghc as build-ghc-x86_64

# RUN ghcup compile ghc -j 8 -v 9.10.2

FROM base

# Carry build args through to this stage

COPY --from=build-ghc-x86_64 /.ghcup /.ghcup
# COPY --from=build-tooling /usr/bin/stack /usr/bin/stack

ENV STACK_VERSION=3.7.1

# Download, verify, and install stack
RUN echo "Downloading and installing stack" &&\
    ghcup install stack ${STACK_VERSION}

# NOTE: 'stack --docker' needs bash + usermod/groupmod (from shadow)
RUN apk add --no-cache bash shadow openssh-client tar gmp gmp-dev binutils-gold ncurses-dev ncurses zlib zlib-dev ncurses-static zlib-static g++

RUN ghcup set ghc ${GHC_VERSION} \
    && stack config set system-ghc --global true

