<script setup lang="ts">
import { API } from "~/api/api";
const route = useRoute();
const artistId = route.params.id.toString();
const { data: artist } = useQuery(API.getArtist(artistId));
const { data: packagesData } = useInfiniteQuery(
    API.getPackages(
        { artist: artistId },
        { sort: "release_date", order: "desc" },
    ),
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
            class="w-full"
            style="
                padding: 10px;
                display: grid;
                gap: 16px;
                grid-template-columns: repeat(auto-fill, minmax(160px, 1fr));
                grid-auto-flow: column;
                grid-auto-columns: minmax(160px, 1fr);
                overflow-x: auto;
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
        </div>
    </div>
</template>
