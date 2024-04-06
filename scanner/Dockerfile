FROM golang:1.22.1-bullseye as builder
RUN apt-get update -y; apt-get install -y libmediainfo-dev ffmpeg
WORKDIR /app
COPY go.mod go.sum ./
RUN go mod download
COPY ./app ./app
COPY ./pkg ./pkg
RUN GOOS=linux go build -o ./scanner ./app

FROM debian:bullseye-slim as runner
RUN apt-get update -y; apt-get install -y libmediainfo-dev
WORKDIR /app
COPY --from=builder /app/scanner ./
CMD ./scanner -d "${WATCH_DIR}" -c "$CONFIG_DIR"/scanner.json