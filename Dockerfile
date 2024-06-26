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

RUN \
  apt-get install -y --no-install-recommends \
  libsqlite3-dev 


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

# Install the Haskell-Language-Server
RUN \
  ghcup -v install hls --isolate /usr/local/bin/hls --force ${HLS} && \
  ln -s /usr/local/bin/hls/bin/haskell-language-server-wrapper /usr/local/bin/haskell-language-server-wrapper


# docker build -t ask2elle-image --build-arg GIT_USERNAME=xxxxx --build-arg GIT_TOKEN=yyy .
# Beoth askelle repo and ask2elle repo are private repos. Therefore, we need to provide the credentials to access them.
ARG GIT_USERNAME
ARG GIT_TOKEN
RUN git config --global credential.helper store
RUN echo "https://${GIT_USERNAME}:${GIT_TOKEN}@github.com" > /root/.git-credentials


RUN \ 
  git clone --branch ideas-bastiaan https://github.com/ideas-edu/ideas /app/ideas && \
  git clone https://github.com/alexgerdes/lvm /app/lvm/ && \
  git clone https://github.com/alexgerdes/Top /app/Top/ && \
  git clone https://github.com/alexgerdes/helium /app/helium/ && \
  git clone --branch hardcoded-exercises https://github.com/alexgerdes/askelle /app/askelle/ 

WORKDIR /app/


RUN cat <<EOL > cabal.project
packages:
        helium/
        ideas/
        lvm/
        Top/
        askelle/
EOL

RUN \ 
  cabal install lvm --overwrite-policy=always && \
  cabal install Top --overwrite-policy=always && \
  cabal install helium --overwrite-policy=always && \
  cabal install askelle --overwrite-policy=always

# Add above built binaries to the PATH
ENV PATH="/root/.local/bin:${PATH}"


#Run the heliumpath command, extract the share path, and execute make in the share/lib directory
RUN \
  heliumpath_output=$(heliumpath) && \
  share_path=$(echo "$heliumpath_output" | grep 'share$') && \
  cd "$share_path/lib" && \
  make

RUN \ 
  rm cabal.project



WORKDIR /app/askelle

RUN askelle.cgi --all-scripts

WORKDIR /app/ask2elle

COPY . /app/ask2elle 


RUN \ 
  cabal build all  


RUN \ 
  cabal test 


