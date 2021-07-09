# syntax=docker/dockerfile:1

FROM ghcr.io/stereoknife/haskell-aarch64:main AS build
SHELL ["/bin/sh", "-c"]
ARG jobs=4

WORKDIR /var/build

RUN cabal update
COPY *.cabal .
COPY cabal.* .

RUN cabal build cowbot-lib \
	--jobs=$jobs \
	--only-dependencies \
	--enable-split-sections \
	--enable-split-objs \
	--enable-executable-stripping \
	--enable-library-stripping \
	--disable-tests \
	--disable-documentation

COPY *.hs .
COPY src/ src/
COPY app/ app/
RUN cabal build exe:cowbot \
	--jobs=$jobs \
	--only-dependencies \
	--enable-split-sections \
	--enable-split-objs \
	--enable-executable-stripping \
	--enable-library-stripping \
	--disable-tests \
	--disable-documentation
	
# dist-newstyle/build/x86_64-linux/ghc-8.10.2/cowbot-0.1.0.0/x/cowbot-bin


FROM arm64v8/alpine:latest

WORKDIR /var/bot
COPY --from=build /var/build/dist-newstyle/build/aarch64-linux/ghc-8.10.5/cowbot-0.1.0.0/x/cowbot ./cowbot
CMD ["./cowbot"]
