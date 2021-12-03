# Cowbot 4

A dumb discord bot written in haskell because I don't value my free time.

## Running from Docker

An AArch64 image is available in this repository's packages. Simply run
```bash
docker run -d \
    -e DISCORD_TOKEN=<your-token> \
    -e GOOGLE_KEY=<your-key> \
    ghcr.io/stereoknife/cowbot:slim
```
to start the bot.

### Building image from source

If you want to run on x86 or otherwise want to build it from source just build the repo's Dockerfile.

```
git clone https://github.com/stereoknife/cowbot-hs
cd cowbot-hs
docker build .
```

If you're running x86 you can uncomment uwuify on the Dockerfile.

## Building with GHC

### AArch64

Make sure LLVM is installed and on your PATH. If running on an M1 Mac you can install it with

`brew install llvm@12`

This doesn't put it on the PATH due to possible conflicts with Apple's bundled LLVM, so you have to specify the location of the binaries separately.

`cabal build -extra-prog-path=/opt/homebrew/opt/llvm@12/bin cowbot`

### Other

`cabal build cowbot`