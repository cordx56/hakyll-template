FROM ghcr.io/cordx56/hakyll-builder-image:main
COPY . .
RUN cabal install /
