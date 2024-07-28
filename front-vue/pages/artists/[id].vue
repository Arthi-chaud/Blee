<script setup lang="ts">
import { API } from "~/api/api";
const route = useRoute();
const artistId = route.params.id.toString();
const { data: artist } = useQuery(API.getArtist(artistId));
const {
    data: packagesData,
    hasNextPage,
    fetchNextPage,
    isFetchingNextPage
} = useInfiniteQuery(
    API.getPackages(
        { artist: artistId },
        { sort: "release_date", order: "desc" },
    ),
);
useInfiniteScroll(
    document ? document.getElementById("scroller") : undefined,
    async () => {
        if (hasNextPage) {
            await fetchNextPage();
        }
    },
    { distance: 100, canLoadMore: () => hasNextPage.value, direction: "right" },
);
</script>
<template>
    <div class="h-full w-full max-w-screen-md">
        <ResourcePageHeader
            :poster="artist?.poster"
            :resource-name="artist?.name"
        />
        <p class="prose-lg">Movies</p>
        <div
            id="scroller"
            class="w-full p-3 grid gap-3 grid-flow-col overflow-scroll"
            style="
                grid-template-columns: repeat(auto-fill, minmax(160px, 1fr));
                grid-auto-columns: minmax(160px, 1fr);
            "
        >
            <PackageItem
                v-for="item in packagesData?.pages.at(0)?.items"
                :key="item.id"
                :package="item"
                :format-secondary-title="
                    (p) => p.release_year?.getFullYear().toString() ?? ''
                "
            />
            <PackageItem v-if="isFetchingNextPage" :package="undefined" />
        </div>
    </div>
</template>
