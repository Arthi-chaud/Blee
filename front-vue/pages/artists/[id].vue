<script setup lang="ts">
import { API } from "~/api/api";
const route = useRoute();
const artistId = route.params.id.toString();
const { data: artist } = useQuery(API.getArtist(artistId));
const { data: artistExternalIds } = useInfiniteQuery(
    API.getExternalIds({ artist: artistId }),
);
const packagesQuery = API.getPackages(
    { artist: artistId },
    { sort: "release_date", order: "desc" },
);
const extrasQuery = API.getExtras(
    { artist: artistId },
    { sort: "name", order: "asc" },
);
const extras = useInfiniteQuery(extrasQuery);
const hasExtras = computed(() =>
    !extras.data.value
        ? true
        : (extras.data.value.pages.at(0)?.items ?? []).length > 0,
);
const externalIdList = computed(() =>
    artistExternalIds.value?.pages.at(0)?.items.slice(0, 3),
);
const artistDescription = computed(() => {
    const firstPage = artistExternalIds.value?.pages.at(0);

    if (firstPage === undefined) {
        return undefined;
    }
    return firstPage.items.find((e) => e.description)?.description ?? null;
});
</script>
<template>
    <div class="h-full w-full max-w-screen-md">
        <ResourcePageHeader
            :poster="artist?.poster"
            :resource-name="artist?.name"
            :brief="artistDescription"
        />
        <p class="prose-lg">Movies</p>
        <InfiniteScroll
            v-slot="{ item }"
            :query="packagesQuery"
            type="poster"
            direction="horizontal"
        >
            <PackageItem :package="item" />
        </InfiniteScroll>
        <p v-if="hasExtras" class="prose-lg">Extras</p>
        <InfiniteScroll
            v-slot="{ item }"
            :query="extrasQuery"
            type="thumbnail"
            direction="horizontal"
        >
            <ExtraItem
                :extra="item"
                :format-secondary-title="(e) => formatDuration(e.duration)"
            />
        </InfiniteScroll>
        <div
            v-if="(externalIdList ?? []).length > 0 "
            class="w-full overflow-x-scroll flex justify-start pb-5"
        >
            <span class="prose-lg">More info on</span>
            <NuxtLink
                v-for="externalLink in externalIdList"
                :key="externalLink.value"
                :to="externalLink.url"
                class="badge badge-primary ml-2 mt-1.5"
            >
                {{ externalLink.provider_name }}
            </NuxtLink>
        </div>
    </div>
</template>
