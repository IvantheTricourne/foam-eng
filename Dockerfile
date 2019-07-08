FROM haskell:8
RUN mkdir -p /usr/src/app
WORKDIR /usr/src/app
COPY . /usr/src/app
RUN stack build && stack install
CMD /root/.local/bin/foam-eng-exe
