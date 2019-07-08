FROM haskell:8
RUN stack build && stack install
CMD ["foam-eng-exe"]