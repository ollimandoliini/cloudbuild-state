FROM utdemir/ghc-musl:v21-ghc8107 as builder
COPY cloudbuild-state.cabal stack.yaml stack.yaml.lock /opt/build/
WORKDIR /opt/build
RUN cabal new-update
RUN cabal new-build --dependencies-only

COPY . /opt/build/

RUN mkdir /workspace
RUN cabal new-build --enable-executable-static

FROM alpine
COPY --from=builder /opt/build/dist-newstyle/build/x86_64-linux/ghc-8.10.7/cloudbuild-state-0.1.0.0/x/cloudbuild-state/build/cloudbuild-state/cloudbuild-state /build/cloudbuild-state

CMD mv /build/cloudbuild-state /workspace/cloudbuild-state
