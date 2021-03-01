# bad-api-assignment

Elm app for Reaktor's summer job 2021 assignment.

Running at [Forge](https://bad-api-assignment.getforge.io/).

Deploy locally (requires the Elm compiler, see [here](https://guide.elm-lang.org/install/elm.html) for instructions:

    git clone https://github.com/jiikai/bad-api-assignment.git
    cd bad-api-assignment
    elm make Main.elm

This yields an `index.html` that contains all of the code.
Serve it with for example [http-server](https://www.npmjs.com/package/http-server):
     
    # run with sudo of required
    npm install -g http-server
    http-server --cors

Ran in the app directory, this will serve `index.html` on `localhost:8080` by default.

The test folder includes a script for 

  1. fetching the JSON resources from the remote APIs and concatenating these to one huge `mockdb.json`;
  2. serving them with [json-server](https://github.com/typicode/json-server) at `localhost:3030`.

To fetch: 

    ./mockdb_server.sh https://bad-api-assignment.reaktor.com/v2 beanies,facemasks,gloves
  
To start server (assuming `mockdb.json` exists): 

    mockdb.json
