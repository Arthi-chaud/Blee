<script setup lang="ts">
import { API } from "~/api/api";

const query = API.getArtists({ sort: "name", order: "asc" });
</script>
<template>
    <InfiniteScroll
        v-slot="{ item }"
        :query="query"
        type="poster"
        direction="vertical"
    >
        <ArtistItem :artist="item" />
    </InfiniteScroll>
</template>
