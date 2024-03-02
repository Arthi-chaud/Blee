FROM rust:1.76-slim-buster as builder
RUN apt update -y && apt install libpq-dev -y
WORKDIR /usr/src/api
COPY . .
RUN cargo install --path api

FROM debian:bullseye-slim as runner
RUN apt update -y && apt install libpq-dev -y
COPY --from=builder /usr/local/cargo/bin/api /usr/local/bin/api
ENV ROCKET_ADDRESS=0.0.0.0
EXPOSE 8000
CMD ["api"]