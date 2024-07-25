<script setup lang="ts">
import { API } from "~/api/api";

const query = API.getArtists({ sortBy: "name", order: "asc" });
</script>
<template>
    <InfiniteScroll v-slot="{ item }" :query="query" type="poster">
        <ArtistItem :artist="item" />
    </InfiniteScroll>
</template>
