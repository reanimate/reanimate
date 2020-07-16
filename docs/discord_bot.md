# Discord bot features

The **reanimate** discord bot has two functions:

 1. Render short animation snippets.
 2. Serve up reanimate documentation.

The bot listens for lines that start with `>>` and renders expressions of
type `Animation`:
<video style="width:100%" muted autoplay loop>
  <source src="https://i.imgur.com/o45cT8r.mp4">
</video>

The length of the animation is capped at 10 seconds. Furthermore, each expression
is only allocated 15 seconds of CPU time and 1GB of memory.

The bot also has access to the latest haddock documentation. Typing `:doc identifier`
will print out the documentation associated with the identifier:
<video style="width:100%" muted autoplay loop>
  <source src="https://i.imgur.com/poeC7S4.mp4">
</video>

https://discord.gg/Qs28Dv6

* Animations
* Documentation

# Continuous deployment

* Github
* Azure pipelines
* Docker Hub
* Static tests
* Watchtower
* Prune
