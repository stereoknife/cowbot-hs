# syntax=docker/dockerfile:1

FROM ghcr.io/stereoknife/haskell-amd64-x-aarch64:main AS build

WORKDIR /var/build

RUN cabal update
COPY *.cabal .
COPY cabal.* .

RUN cabal build cowbot-lib -j1 --only-dependencies

COPY *.hs .
COPY src/ src/
COPY app/ app/
RUN cabal build exe:cowbot -j1
# dist-newstyle/build/x86_64-linux/ghc-8.10.2/cowbot-0.1.0.0/x/cowbot-bin


FROM arm64v8/alpine:latest

WORKDIR /var/bot
COPY --from=build /var/build/dist-newstyle/build/aarch64-linux/ghc-8.10.5/cowbot-0.1.0.0/x/cowbot ./cowbot
CMD ["./cowbot"]
