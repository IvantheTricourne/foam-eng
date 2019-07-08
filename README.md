# foam-eng

## Installation

### Stack

`stack build && stack install`

Then execute the `foam-eng-exe`. Or just run via `stack`:

`stack run`

This exposes a REST server on `0.0.0.0:3000`.

### Docker

Build the docker image from `./Dockerfile`:

`docker build -t user-api -f Dockerfile -m 4g .`

Then, run the `user-api` container:

`docker run user-api`

Alternatively, for `localhost` testing, use Docker's `-p` option to map to your desired local port (e.g. `XXXX`) to port `3000`:

`docker run -p XXXX:3000 user-api`
