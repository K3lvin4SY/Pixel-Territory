# Web version of Pixel Territory: Scala.js compiled to main.js, served as static files by nginx.

# Build on the machine's own platform; the output is plain JavaScript, so it works for every image platform
FROM --platform=$BUILDPLATFORM eclipse-temurin:21-jdk AS build
RUN command -v curl || (apt-get update && apt-get install -y --no-install-recommends curl && rm -rf /var/lib/apt/lists/*)
RUN arch="$(uname -m | sed 's/arm64/aarch64/')" \
 && curl -fsSL "https://github.com/VirtusLab/scala-cli/releases/latest/download/scala-cli-${arch}-pc-linux.gz" \
  | gzip -d > /usr/local/bin/scala-cli \
 && chmod +x /usr/local/bin/scala-cli
WORKDIR /app
COPY src ./src
RUN scala-cli --power package src/shared src/js --server=false -o /out/main.js

FROM nginx:alpine
COPY web/index.html /usr/share/nginx/html/index.html
COPY --from=build /out/main.js /usr/share/nginx/html/main.js
EXPOSE 80
