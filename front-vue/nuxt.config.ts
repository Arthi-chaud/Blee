// https://nuxt.com/docs/api/configuration/nuxt-config
export default defineNuxtConfig({
    compatibilityDate: "2024-04-03",
    devtools: { enabled: true },
    modules: ["@nuxt/eslint"],
    routeRules:
        process.env.NODE_ENV !== "production"
            ? {
                  "/api/**": { proxy: `${process.env.SSR_SERVER_URL!}/**` },
              }
            : {},
});
