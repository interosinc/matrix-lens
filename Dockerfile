FROM haskell
RUN cabal new-update
RUN mkdir -p /src/matrix-lens
ADD . /src/matrix-lens
WORKDIR /src/matrix-lens
RUN cabal new-build
