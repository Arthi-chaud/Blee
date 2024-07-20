<script setup lang="ts">
const homeRoute = "/packages";
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
        <a :href="homeRoute">
            <img src="/icon.png" class="h-10" />
            <span class="ml-4 text-xl">Blee</span>
        </a>
    </div>
    <div class="w-full h-full flex flex-row">
        <div class="hidden md:flex">
            <nav class="sidenav px-2">
                <a
                    class="btn btn-ghost no-animation p-2 !h-auto flex flex-col mb-2"
                    :class="{ 'font-normal': item.path != path }"
                    v-for="item in routes"
                    :href="item.path"
                >
                    <fa :icon="item.icon" />
                    <span class="pt-0.5">
                        {{ item.label }}
                    </span>
                </a>
            </nav>
        </div>
        <div class="w-full h-full">
            <slot />
            <div class="btm-nav md:hidden">
                <a
                    :class="{ active: item.path == path }"
                    v-for="item in routes"
                    :href="item.path"
                >
                    <fa :icon="item.icon" />
                    <span class="pt-0.5">
                        {{ item.label }}
                    </span>
                </a>
            </div>
        </div>
    </div>
</template>

<style>
.sidenav {
    display: flex;
    flex-direction: column;
    height: 100%;
    top: 0;
    left: 0;
}
</style>
