/** @type {import('tailwindcss').Config} */
export default {
    content: [],
    theme: {
        extend: {
            fontFamily: {
                rubik: ["Rubik", "sans-serif"],
            },
        },
    },
    plugins: [require("daisyui")],
};
