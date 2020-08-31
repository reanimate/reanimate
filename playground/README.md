Always run with GHCi.
Forbid TemplateHaskell.
Generate a single frame at a time.
Stop generating frames once a limit is hit:
  * 5 seconds iff there's a queue.
  * 30 seconds if there isn't a queue.
  * 5 megs of disk space used.
  * Framerate hits 30 fps.
  * Code crashes.
  * Code hits memory ceiling.
  * New render request from the same user.
Queue rendering.
Skip queue when cache hit.

Test against:
  * last [1..]
  * let e = error ("error: " ++ e) in e



# How to run locally:

## Backend

```
stack build
stack exec --cwd ../ playground
```

## Frontend

By default the frontend will use the backend running at reanimate.clozecards.com.
To switch to a local backend, change 'backend' in Main.elm to 'Local'.

```
cd viewer-elm
npm install
npm run dev-server
```

# Design

The playground frontend connects to the backend (reanimate.clozecards.com) over HTTPS.
The backend runs nginx to serve static files and as a reverse proxy for the
websocket server. All backend services are run in docker and auto-updated.
SSL certificate is from lets-encrypt.org
Frontend is written in Elm and automatically deployed to reanimate's GitHub Page
on every push to master. The built-in examples are in the 'snippets/' folder
and they are type-checked on every commit to master.
