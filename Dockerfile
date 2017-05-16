FROM haskell:8

WORKDIR /sit
COPY . /sit

RUN cabal update
RUN cabal install

RUN cp ./dist/build/Sit.bin/Sit.bin ./Sit.bin

CMD ["Sit.bin", "test/Test.agda"]
