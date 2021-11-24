# syntax=docker/dockerfile:1

FROM ghcr.io/stereoknife/haskell-aarch64:latest AS build

ARG jobs=4

WORKDIR /var/build

RUN cabal update
COPY *.cabal .
COPY cabal.* .
COPY howdy/ howdy/

RUN cabal build howdy \
	--jobs=$jobs \
	--enable-split-sections \
	--enable-split-objs \
	--enable-executable-stripping \
	--enable-library-stripping \
	--disable-tests \
	--disable-documentation

COPY *.hs .
COPY app/ app/
RUN cabal build cowbot \
	--jobs=$jobs \
	--enable-split-sections \
	--enable-split-objs \
	--enable-executable-stripping \
	--enable-library-stripping \
	--disable-tests \
	--disable-documentation

#RUN cabal install cowbot
CMD ["/bin/bash"]
	
# dist-newstyle/build/aarch64-linux/ghc-8.10.7/cowbot-0.1.0.0/x/cowbot/build/cowbot/cowbot


# FROM arm64v8/alpine:latest

# WORKDIR /var/bot
# COPY --from=build /var/build/dist-newstyle/build/aarch64-linux/ghc-8.10.7/cowbot-0.1.0.0/x/cowbot/build/cowbot/cowbot ./cowbot
# RUN chmod +x ./cowbot \
# 	&& echo "oops guess I leaked my token" >> token.secret
# CMD ["/var/bot/cowbot"]
