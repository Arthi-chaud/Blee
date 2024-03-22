FROM golang:1.22.1-alpine as builder
WORKDIR /app
COPY go.mod go.sum ./
RUN go mod download
COPY *.go ./
RUN CGO_ENABLED=0 GOOS=linux go build -o ./scanner

FROM debian:bullseye-slim as runner
WORKDIR /app
COPY --from=builder /app/scanner ./
CMD ./scanner -d "${WATCH_DIR}"