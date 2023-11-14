FROM ghcr.io/cordx56/hakyll-builder-image:main AS builder
COPY . .
RUN cabal install --installdir=/ --overwrite-policy=always

FROM debian:buster-slim
ENV LC_ALL C.UTF-8
COPY --from=builder /site /
CMD ["/site"]
