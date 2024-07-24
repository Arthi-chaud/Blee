<script setup lang="ts">
import { API } from "~/api/api";

const query = API.getArtists({ sortBy: "name", order: "asc" });

</script>
<template>
    <InfiniteScroll :query="query" type="poster" v-slot="{ item }">
        <ArtistItem :artist="item"/>
    </InfiniteScroll>
</template>
