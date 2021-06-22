# syntax=docker/dockerfile:1

FROM ghcr.io/stereoknife/docker-haskell-arm:main AS deps

WORKDIR /var/build

RUN cabal update
COPY *.cabal .
COPY cabal.*
RUN cabal build discord-haskell
RUN cabal build cowbot-lib --only-dependencies

FROM deps as build

WORKDIR /var/build

COPY *.hs .
COPY src/ src/
COPY app/ app/
RUN cabal build exe:cowbot
# dist-newstyle/build/x86_64-linux/ghc-8.10.2/cowbot-0.1.0.0/x/cowbot-bin


FROM arm64v8/alpine:latest

WORKDIR /var/bot
COPY --from=build /var/build/dist-newstyle/build/x86_64-linux/ghc-8.10.2/cowbot-0.1.0.0/x/cowbot ./cowbot
CMD ["./cowbot"]
