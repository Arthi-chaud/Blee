<script setup lang="ts">
const homeRoute = "/packages";
const routes = [
    { path: "/artists", label: "Artists", icon: "user" },
    { path: "/packages", label: "Movies", icon: "film" },
    { path: "/extras", label: "Videos", icon: "tv" },
    { path: "/settings", label: "Settings", icon: "cog" },
] as const;
const router = useRouter();
const path = computed(() => router.currentRoute.value.path);
</script>
<template>
    <div class="h-screen w-screen overflow-clip">
        <div class="pl-4 navbar">
            <NuxtLink :to="homeRoute" class="navbar-start">
                <img src="/icon.png" class="h-10" />
                <span class="ml-4 text-xl">Blee</span>
            </NuxtLink>
        </div>
        <div class="w-full h-full flex flex-row">
            <div class="hidden sm:flex">
                <nav class="sidenav px-2">
                    <NuxtLink
                        class="btn btn-ghost no-animation p-2 !h-auto flex flex-col mb-2"
                        :class="{ 'font-normal': item.path != path }"
                        v-for="item in routes"
                        :to="item.path"
                    >
                        <fa :icon="item.icon" />
                        <span class="pt-0.5">
                            {{ item.label }}
                        </span>
                    </NuxtLink>
                </nav>
            </div>
            <div class="w-full h-auto overflow-scroll">
                <slot />
            </div>
            <div class="btm-nav sm:hidden">
                <NuxtLink
                    :class="{ active: item.path == path }"
                    v-for="item in routes"
                    :to="item.path"
                >
                    <fa :icon="item.icon" />
                    <span class="pt-0.5">
                        {{ item.label }}
                    </span>
                </NuxtLink>
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
