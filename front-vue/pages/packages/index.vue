<script setup lang="ts">
import { API } from "~/api/api";
import { useInfiniteQuery } from "~/composables/queries";
import { useInfiniteScroll } from "@vueuse/core";

const { data, suspense, fetchNextPage, hasNextPage } = useInfiniteQuery(
    API.getPackages({}, { sortBy: "name", order: "asc" }),
);
const items = computed(() =>
    data.value?.pages.map(({ items }) => items).flat(),
);
console.log(items.value);
const el = ref<HTMLElement | null>(null);
const _ = useInfiniteScroll(
    el,
    async () => {
        console.log("A");
        if (hasNextPage) {
            await fetchNextPage();
        }
    },
    { distance: 10 },
);

onServerPrefetch(async () => {
    await suspense();
});
</script>
<template>
    <div class="w-full h-auto flex justify-center">
        <div class="poster-grid h-auto max-w-screen-xl p-2" ref="el">
            <PackageItem
                :package="item"
                v-for="item in items"
                :key="item.id"
                class="h-full"
            />
        </div>
    </div>
</template>
