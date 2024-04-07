# Loosely based on https://www.fpcomplete.com/blog/2017/12/building-haskell-apps-with-docker
FROM haskell:9.4.8-slim as base

FROM base as dependencies
RUN mkdir /opt/build
WORKDIR /opt/build
# Docker build should not use cached layer if any of these is modified
COPY stack.yaml package.yaml stack.yaml.lock /opt/build/
RUN stack build --dependencies-only

# -------------------------------------------------------------------------------------------
FROM base as build
# Copy compiled dependencies from previous stage
COPY --from=dependencies /root/.stack /root/.stack
COPY . /opt/build/
WORKDIR /opt/build
RUN stack build
RUN mv "$(stack path --local-install-root --system-ghc)/bin" /opt/build/bin

# -------------------------------------------------------------------------------------------
# Base image for stack build so compiled artifact from previous
# stage should run
# We use the same base image as the base of the haskell image
FROM debian:buster-slim as runner
RUN mkdir -p /opt/app
WORKDIR /opt/app
COPY --from=build /opt/build/bin /opt/app
CMD ["./matcher-exe"]