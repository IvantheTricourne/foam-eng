FROM haskell:8
RUN mkdir -p /usr/src/app
WORKDIR /usr/src/app
COPY foam-eng.cabal /usr/src/app
COPY stack.yaml /usr/src/app
RUN stack build && stack install
