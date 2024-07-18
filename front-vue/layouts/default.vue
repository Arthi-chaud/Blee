<script setup lang="ts">
import { faCog, faUser, faFilm, faTv } from "@fortawesome/free-solid-svg-icons";

const routes = ref([
    { path: "/artists", label: "Artists", icon: faUser },
    { path: "/packages", label: "Movies", icon: faFilm },
    { path: "/extras", label: "Videos", icon: faTv },
    { path: "/settings", label: "Settings", icon: faCog },
]);
const router = useRouter();
const path = computed(() => router.currentRoute.value.path);
</script>
<template>
    <div class="ml-2 navbar bg-base-100">
        <img src="/icon.png" class="h-10" />
        <a class="ml-2 btn btn-ghost text-xl no-animation">Blee</a>
    </div>
    <li v-for="item in routes">
        {{ item.label }}
        <fa :icon="item.icon" />
    </li>
    <slot />
</template>
