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



