<script setup lang="ts">
const routes = ref([
    { path: "/artists", label: "Artists", icon: "user" },
    { path: "/packages", label: "Movies", icon: "film" },
    { path: "/extras", label: "Videos", icon: "tv" },
    { path: "/settings", label: "Settings", icon: "cog" },
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
