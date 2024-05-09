# SRC: https://github.com/edwardinubuntu/flutter-web-dockerfile/blob/master/Dockerfile
FROM debian:latest AS build-env

RUN apt-get update && apt-get install -y curl git unzip

ARG FLUTTER_SDK=/usr/local/flutter
ARG FLUTTER_VERSION=stable
ARG APP=/app/

RUN git clone https://github.com/flutter/flutter.git $FLUTTER_SDK
RUN cd $FLUTTER_SDK && git fetch && git checkout $FLUTTER_VERSION

ENV PATH="$FLUTTER_SDK/bin:$FLUTTER_SDK/bin/cache/dart-sdk/bin:${PATH}"

RUN mkdir $APP
COPY . $APP
WORKDIR $APP

RUN flutter clean
RUN flutter pub get
RUN dart run build_runner build --delete-conflicting-outputs
RUN flutter build web --release --source-maps

FROM nginx:1.25.2-alpine
COPY --from=build-env /app/build/web /etc/nginx/html
COPY ./nginx.conf.template /etc/nginx/templates/front.conf.template
CMD ["nginx", "-g", "daemon off;"]