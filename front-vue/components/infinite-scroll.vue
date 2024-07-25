<script setup lang="ts" generic="T extends Resource">
import { API } from "~/api/api";
import type { ImageType } from "~/models/domain/image";
import type { Resource } from "~/models/domain/resource";
import type { PaginatedQuery } from "~/models/queries";

const { query } = defineProps<{
    query: PaginatedQuery<T>;
    type: ImageType;
}>();
defineSlots<{
    default(props: { item: T | undefined }): unknown;
}>();
const { data, hasNextPage, fetchNextPage } = useInfiniteQuery(query);

const itemsCount = computed(
    () =>
        data.value?.pages
            .map(({ items }) => items.length)
            .reduce((a, b) => a + b, 0) ?? 0,
);
const getItem = computed(() => (itemIndex: number) => {
    const item = data.value?.pages
        .at(Math.floor(itemIndex / API.defaultPageSize))
        ?.items.at(itemIndex % API.defaultPageSize);
    return item;
});
const skeletons = computed(() => (hasNextPage ? [1, 2] : []));
useInfiniteScroll(
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
        <div
            class="w-full h-full max-w-screen-xl p-2"
            :class="{
                'poster-grid': type === 'poster',
                'thumbnail-grid': type === 'thumbnail',
            }"
        >
            <slot v-for="n in itemsCount" v-bind="{ item: getItem(n - 1) }" />
            <template v-if="hasNextPage">
                <slot v-for="_ in skeletons" v-bind="{ item: undefined }" />
            </template>
        </div>
    </div>
</template>
