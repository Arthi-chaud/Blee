/** @type {import('tailwindcss').Config} */
export default {
    daisyui: {
        themes: [
            {
                light: {
                    ...require("daisyui/src/theming/themes")["light"],
                    primary: "#006972",
                    secondary: "#4a6366",
                    accent: "#515e7d",
                    neutral: "#f4ffff",
                    "base-100": "#f5fafb",
                    info: "#41ffff",
                    success: "#9affdc",
                    warning: "#fff129",
                    error: "#ba1a1a",
                    background: "#f5fafb",
                },
                dark: {
                    ...require("daisyui/src/theming/themes")["dark"],
                    primary: "#81D3DF",
                    secondary: "#b1cbcf",
                    accent: "#b9c6ea",
                    neutral: "#f4ffff",
                    "base-100": "#0e1415",
                    info: "#41ffff",
                    success: "#9affdc",
                    warning: "#fff129",
                    error: "#ba1a1a",
                    background: "#0e1415",
                },
            },
        ],
    },
    theme: {
        extend: {
            flex: {
                2: "2 2 0%",
            },
            fontFamily: {
                rubik: ["Rubik", "sans-serif"],
            },
        },
    },
    plugins: [require("daisyui"), require("@tailwindcss/typography")],
};
