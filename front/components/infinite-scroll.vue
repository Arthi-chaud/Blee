<script setup lang="ts" generic="T extends Resource">
import { API } from "~/api/api";
import type { ImageType } from "~/models/domain/image";
import type { Resource } from "~/models/domain/resource";
import type { PaginatedQuery } from "~/models/queries";

const { query, direction } = defineProps<{
    query: PaginatedQuery<T>;
    type: ImageType;
    direction: "vertical" | "horizontal";
}>();
defineSlots<{
    default(props: { item: T | undefined }): unknown;
}>();
const { data, hasNextPage, fetchNextPage, isFetching, isFetchingNextPage } =
    useInfiniteQuery(query);

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
const scrollableRef = ref(null);
useInfiniteScroll(
    scrollableRef,
    async () => {
        if (hasNextPage) {
            await fetchNextPage();
        }
    },
    {
        distance: 100,
        canLoadMore: () => hasNextPage.value,
        direction: direction === "vertical" ? "bottom" : "right",
    },
);
</script>
<template>
    <div
        v-if="direction === 'vertical'"
        class="w-full h-auto flex justify-center"
    >
        <div
            ref="scrollableRef"
            class="w-full h-full max-w-screen-xl p-2 overflow-scroll"
            :class="{
                'poster-grid': type === 'poster',
                'thumbnail-grid': type === 'thumbnail',
            }"
        >
            <slot v-for="n in itemsCount" v-bind="{ item: getItem(n - 1) }" />
            <template v-if="isFetchingNextPage || isFetching">
                <slot v-for="_ in skeletons" v-bind="{ item: undefined }" />
            </template>
        </div>
    </div>
    <div v-else>
        <div
            ref="scrollableRef"
            class="w-full pl-2 py-2 grid gap-3 grid-flow-col overflow-scroll"
            style="
                grid-template-columns: repeat(auto-fill, minmax(160px, 1fr));
                grid-auto-columns: minmax(160px, 1fr);
            "
        >
            <slot v-for="n in itemsCount" v-bind="{ item: getItem(n - 1) }" />
            <template v-if="isFetchingNextPage || isFetching">
                <slot v-for="_ in skeletons" v-bind="{ item: undefined }" />
            </template>
        </div>
    </div>
</template>
