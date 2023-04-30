# syntax=docker/dockerfile:1

FROM haskell:9.4 AS build

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

COPY . .

RUN cabal build cowbot \
	--jobs=$jobs \
	--enable-split-sections \
	--enable-split-objs \
	--enable-executable-stripping \
	--enable-library-stripping \
	--disable-tests \
	--disable-documentation

RUN cabal install cowbot \
	--install-method=copy \
	--overwrite-policy=always

RUN apt-get update && \
	apt-get install --no-install-recommends -y upx

RUN strip /root/.cabal/bin/cowbot \
	&& upx /root/.cabal/bin/cowbot

CMD ["/bin/bash"]

# FROM rust:1.55 as uwu-build

# RUN cargo install uwuify

FROM debian:stable-slim

WORKDIR /var/bot

RUN apt-get update && \
	apt-get install --no-install-recommends -y \
	zlib1g \
	libnuma1 \
	ca-certificates

COPY --from=build /root/.cabal/bin/cowbot ./cowbot
# COPY --from=uwu-build /root/.cargo/bin/uwuify ./uwuify

ENV PATH="/var/bot:${PATH}"

RUN chmod +x ./cowbot

CMD ["/var/bot/cowbot"]
