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
    <div class="w-screen h-screen">
        <div class="pl-4 navbar">
            <NuxtLink :to="homeRoute" class="navbar-start">
                <img src="/icon.png" class="h-10" />
                <span class="ml-4 text-xl">Blee</span>
            </NuxtLink>
        </div>
        <div
            class="w-full flex flex-row"
            :style="{
                // Navbar somehow fucks the height
                height: 'calc(100vh - 4rem)',
            }"
        >
            <div class="hidden sm:flex">
                <nav class="sidenav px-2">
                    <NuxtLink
                        v-for="item in routes"
                        :key="item.path"
                        class="btn btn-ghost no-animation p-2 !h-auto flex flex-col mb-2"
                        :class="{ 'font-normal': item.path != path }"
                        :to="item.path"
                    >
                        <fa :icon="item.icon" />
                        <span class="pt-0.5">
                            {{ item.label }}
                        </span>
                    </NuxtLink>
                </nav>
            </div>
            <div
                id="el"
                class="w-full h-auto flex justify-center overflow-scroll p-3 xl:p-0"
            >
                <slot />
            </div>
            <div class="btm-nav sm:hidden">
                <NuxtLink
                    v-for="item in routes"
                    :key="item.path"
                    :class="{ active: item.path == path }"
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
