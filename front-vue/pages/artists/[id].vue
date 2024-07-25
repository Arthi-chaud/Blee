<script setup lang="ts">
import { API } from "~/api/api";
const route = useRoute();
const { data: artist, suspense } = useQuery(
    API.getArtist(route.params.id.toString()),
);
</script>
<template>
    <div>Artist {{ artist?.name }}</div>
</template>
