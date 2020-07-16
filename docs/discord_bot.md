# Discord bot overview

The **reanimate** discord bot has two functions:

 1. Render short animation snippets.
 2. Serve up reanimate documentation.

The bot listens for lines that start with `>>` and renders expressions of
type `Animation`:

<video style="width:100%; max-width: 640px" muted autoplay loop>
  <source src="https://i.imgur.com/o45cT8r.mp4">
</video>

The length of the animation is capped at 10 seconds. Furthermore, each expression
is only allocated 15 seconds of CPU time and 1GB of memory.

The bot also has access to the latest haddock documentation. Typing `:doc identifier`
will print out the documentation associated with the identifier:

<video style="width:100%; max-width: 640px" muted autoplay loop>
  <source src="https://i.imgur.com/poeC7S4.mp4">
</video>



The discord server is open to everyone so feel free to join and have a chat with the bot: <https://discord.gg/Qs28Dv6>

# Continuous deployment

Reanimate is a rapidly moving target and the bot would quickly get out-of-date
if it wasn't automatically updated. Keeping the bot up-to-date happens in 6 steps:

1. Source management: Github
Code changes to start out as pull-requests on [GitHub](https://github.com/reanimate/reanimate/pulls). Here the changes will be reviewed and tested before they can be merged into the main branch.

2. Regression testing: Azure Pipelines
Reanimate is a multi-platform library and has to work on Linux, Mac OS, and Windows. To make sure there are no regressions, [Azure Pipelines](https://dev.azure.com/lemmih0612/reanimate/_build?definitionId=2&_a=summary) is used run the test suite cross all platforms and with several different configurations.

3. Image building: Docker Cloud
Once a pull-request has been merged into the main branch, an image build is triggered on [Docker Hub](https://hub.docker.com/repository/docker/reanimate/discord-bot). This image will contain the latest version of reanimate as well as the fully configured discord bot, ready to be deployed.

4. Static tests
Before the image is uploaded to Docker Hub, several consistency checks are performed to ensure that animations can be rendered, the documentation has been built, and external dependencies are available. If any of the consistency checks fail then the image will be discarded.

5. Watchtower
When a new image becomes available, watchtower will automatically stop the old container and start the new bot. Watchtower does not receive push notifications from Docker Hub but instead looks for updates every 5 minutes, adding a bit of latency to the pipeline.

6. Prune
The last step is the periodically prune old images and containers from docker. Without this step, old date would slowly accumulate until the disk drive is full.

These 6 steps together make up a pipeline that ensures the reanimate discord bot is always running with the latest code and documentation. The time from a patch landing in the main branch until a new bot is deployed is roughly 30 minutes and requires no human intervention.
