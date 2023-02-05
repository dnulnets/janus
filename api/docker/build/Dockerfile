FROM haskell:9.2.5-slim

WORKDIR /opt/janus

COPY ./api.cabal /opt/janus/api.cabal
COPY ./stack.yaml /opt/janus/stack.yaml

RUN stack build --only-dependencies