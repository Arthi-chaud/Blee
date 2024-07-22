<script setup lang="ts">
import { API } from "~/api/api";
import { useInfiniteQuery } from "~/composables/queries";

const { data, hasNextPage, fetchNextPage } = useInfiniteQuery(
    API.getPackages({}, { sortBy: "name", order: "asc" }),
);

const items = computed(() => 
    data.value?.pages.map(({ items }) => items).flat(),
);
const skeletons = computed(() => hasNextPage ? [1, 2] : [])
const _ = useInfiniteScroll(
    //TODO Make this cleaner
    document ? document.getElementById("el") : undefined,
    async () => {
        if (hasNextPage) {
            await fetchNextPage();
        }
    },
    { distance: 100, canLoadMore: () => hasNextPage.value },
);
</script>
<template>
    <div class="w-full h-auto flex justify-center">
        <div class="poster-grid h-auto max-w-screen-xl p-2">
            <PackageItem
                :package="item"
                v-for="item in items"
                :key="item.id"
                class="h-full"
            />
            <PackageItem
                :package="undefined"
                v-if="hasNextPage"
                v-for="_ in skeletons"
                class="h-full"
            />
        </div>
    </div>
</template>
