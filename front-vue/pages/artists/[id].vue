<script setup lang="ts">
import { API } from "~/api/api";
const route = useRoute();
const { data: artist } = useQuery(API.getArtist(route.params.id.toString()));
</script>
<template>
    <ResourcePageHeader
        :poster="artist?.poster"
        :resource-name="artist?.name"
    />
</template>
