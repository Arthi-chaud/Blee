// https://nuxt.com/docs/api/configuration/nuxt-config
export default defineNuxtConfig({
    compatibilityDate: "2024-04-03",
    devtools: { enabled: true },
    modules: [
        "@nuxt/eslint",
        "@nuxtjs/tailwindcss",
        "@vesp/nuxt-fontawesome",
        "@vueuse/nuxt",
        "@nuxt/eslint",
    ],
    css: ["@/assets/css/main.css"],
    app: {
        head: {
            title: "Blee",
        },
    },
    components: [
        {
            path: "~/components",
            pathPrefix: false,
        },
    ],
    routeRules: {
        '/player/**': { ssr: false },
        "/api/**": { proxy: `${process.env.SSR_SERVER_URL!}/**` },
        "/transcoder/**": { proxy: `${process.env.SSR_TRANSCODER_URL!}/**` },
        "/scanner/**": { proxy: `${process.env.SSR_SCANNER_URL!}/**` },
    },
    fontawesome: {
        component: "fa",
        icons: {
            regular: ["star"],
            solid: [
                "arrows-rotate",
                "play",
                "cog",
                "user",
                "film",
                "tv",
                "star",
                "star-half-stroke",
            ],
        },
    },
});
