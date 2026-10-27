# Pixel Territory
A 2D scala multiplayer game where players compete to claim as much territory as possible on a grid-based map.
Players move their character around the map, leaving behind a trail that fills in the area they cover.
The goal is to capture territory while defending against other players trying to do the same.

However, if another player runs into your trail, you lose a life and the territory you've captured is lost.
This adds an element of risk as players must balance expanding their territory with avoiding confrontation.

## Start game
Desktop (needs [scala-cli](https://scala-cli.virtuslab.org)):
```
scala-cli run .
```

Web version (Scala.js), then open `web/index.html` through any static file server:
```
scala-cli --power package src/shared src/js -o web/main.js -f
cd web && python3 -m http.server 8080
```

Or run the web version with Docker:
```
docker compose up -d
```
It's available on http://localhost:8080.

## Project layout
- `src/shared`: the game itself, shared by both versions
- `src/jvm`: desktop window (Swing) and main loop
- `src/js`: browser window (canvas) and main loop

## Releases
Pushing a tag like `v1.0.0` runs `.github/workflows/release.yml`. It builds desktop apps for
Windows, Linux and macOS (Intel + Apple Silicon) and publishes the web version as the Docker image
`ghcr.io/k3lvin4sy/pixel-territory`.
