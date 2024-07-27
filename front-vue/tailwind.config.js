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
                    //todo: copy values from https://github.com/Arthi-chaud/Blee/blob/40565299ac7e7ecadad94c71b409db44849c336e/front/lib/theme.dart
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
