FROM node:18-alpine3.18 AS base
FROM base AS builder
WORKDIR /app
# Dependencies
COPY ./*.json ./.yarnrc.yml ./*.lock  ./
COPY ./.yarn ./.yarn
RUN yarn workspaces focus

# Build
COPY ./app.vue ./
COPY ./nuxt.config.ts ./
COPY ./tailwind.config.js ./
COPY ./tsconfig.json ./
COPY ./api ./api
COPY ./assets ./assets
COPY ./components ./components
COPY ./composables ./composables 
COPY ./layouts ./layouts
COPY ./models ./models
COPY ./pages ./pages 
COPY ./plugins ./plugins
COPY ./public ./public
COPY ./utils ./utils
RUN yarn run nuxt prepare && yarn run nuxt build

FROM base as runner
WORKDIR /app

ENV NODE_ENV production
ENV NITRO_PORT 3000
ENV NITRO_HOST "0.0.0.0"

COPY --from=builder /app/.output ./.output

CMD node .output/server/index.mjs
