FROM haskell:8
WORKDIR /build
ADD *.hs ./
ADD Makefile ./
ADD Roo.cabal ./
RUN make
ENTRYPOINT ["./Roo"]

