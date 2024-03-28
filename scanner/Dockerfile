FROM golang:1.22.1-alpine as builder
WORKDIR /app
COPY go.mod go.sum ./
RUN go mod download
COPY ./app ./app
COPY ./pkg ./pkg
RUN CGO_ENABLED=0 GOOS=linux go build -o ./scanner ./app

FROM debian:bullseye-slim as runner
WORKDIR /app
COPY --from=builder /app/scanner ./
CMD ./scanner -d "${WATCH_DIR}" -c "$CONFIG_DIR"/scanner.json