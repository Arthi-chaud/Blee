<script setup lang="ts">
import { API } from "~/api/api";
const route = useRoute();
const artistId = route.params.id.toString();
const { data: artist } = useQuery(API.getArtist(artistId));
const packagesQuery = API.getPackages(
    { artist: artistId },
    { sort: "release_date", order: "desc" },
);
const extrasQuery = API.getExtras(
    { artist: artistId },
    { sort: "name", order: "asc" },
);
</script>
<template>
    <div class="h-full w-full max-w-screen-md">
        <ResourcePageHeader
            :poster="artist?.poster"
            :resource-name="artist?.name"
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
        <p class="prose-lg">Extras</p>
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
    </div>
</template>
