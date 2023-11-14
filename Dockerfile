FROM ghcr.io/cordx56/hakyll-builder-image:main AS builder
COPY . .
RUN cabal install --installdir=/ --overwrite-policy=always

FROM debian:buster-slim
COPY --from=builder /site /
CMD ["/site"]
