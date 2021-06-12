# syntax=docker/dockerfile:1

FROM haskell:8.10.2 AS build
WORKDIR /var/build
RUN ["cabal", "update"]
COPY *.cabal .
RUN ["cabal", "build", "cowbot-lib", "--only-dependencies"]
COPY *.hs .
COPY src/ src/
COPY app/ app/
RUN [ "cabal", "build", "exe:cowbot" ]
# RUN [ "cabal", "install", "exe:cowbot", "--disable-tests", "--install-method=copy", "--installdir=./dist" ]
# dist-newstyle/build/x86_64-linux/ghc-8.10.2/cowbot-0.1.0.0/x/cowbot-bin

FROM alpine:latest
WORKDIR /var/bot
COPY --from=build /var/build/dist-newstyle/build/x86_64-linux/ghc-8.10.2/cowbot-0.1.0.0/x/cowbot ./cowbot
CMD ["./cowbot"]