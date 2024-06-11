# FROM ubuntu:22.04

# RUN ["apt-get", "update"]

# RUN ["apt-get", "install", "-y", "build-essential", "curl", "libffi-dev", "libffi8ubuntu1", "libgmp-dev", "libgmp10", "libncurses-dev", "libncurses5", "libtinfo5"]
# # Download dependencies for ghcup 
# RUN ["sh", "-c", "curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | BOOTSTRAP_HASKELL_NONINTERACTIVE=1 BOOTSTRAP_HASKELL_INSTALL_NO_STACK=1 BOOTSTRAP_HASKELL_GHC_VERSION=9.2.8"]

# CMD ["/bin/bash"]
# # RUN ["apt-get", "install", "-y", "fd-find git make zlib1g"]

# # WORKDIR /ask2elle

# # COPY . .

# # ENV PORT=8080

# # EXPOSE 8080

FROM ubuntu:focal

ARG DEBIAN_FRONTEND=noninteractive
ENV TZ=Europe/Berlin

# install General Dependencies 
RUN \ 
  apt-get update -y && \
  apt-get install -y --no-install-recommends \
  fd-find \
  libncurses5-dev

# install Haskell-Related Dependencies
RUN \
  apt-get update -y && \
  apt-get install -y --no-install-recommends \
  curl \
  libnuma-dev \
  zlib1g-dev \
  libgmp-dev \
  libgmp10 \
  git \
  wget \
  lsb-release \
  software-properties-common \
  gnupg2 \
  apt-transport-https \
  gcc \
  autoconf \
  automake \
  build-essential


# install gpg keys

# install ghcup
RUN \
  curl https://downloads.haskell.org/~ghcup/x86_64-linux-ghcup > /usr/bin/ghcup && \
  chmod +x /usr/bin/ghcup

ARG GHC=9.2.8
ARG CABAL=3.10.1.0
ARG HLS=2.8.0.0
# install GHC and cabal

RUN \
  ghcup -v install ghc --isolate /usr/local --force ${GHC} && \
  ghcup -v install cabal --isolate /usr/local/bin --force ${CABAL} && \ 
  cabal update


RUN \
  ghcup -v install hls --isolate /usr/local/bin/hls --force ${HLS} && \
  ln -s /usr/local/bin/hls/bin/haskell-language-server-wrapper /usr/local/bin/haskell-language-server-wrapper



WORKDIR /ask2elle

COPY . .

RUN \ 
  cabal run ask2elle 