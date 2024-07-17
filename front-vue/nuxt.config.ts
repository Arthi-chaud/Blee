// https://nuxt.com/docs/api/configuration/nuxt-config
export default defineNuxtConfig({
    compatibilityDate: "2024-04-03",
    devtools: { enabled: true },
    modules: ["@nuxt/eslint", "@nuxtjs/tailwindcss"],
    components: [
        {
            path: "~/components",
            pathPrefix: false,
        },
    ],
    routeRules:
        process.env.NODE_ENV !== "production"
            ? {
                  "/api/**": { proxy: `${process.env.SSR_SERVER_URL!}/**` },
              }
            : {},
});
