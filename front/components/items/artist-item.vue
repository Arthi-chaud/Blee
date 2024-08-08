<script setup lang="ts">
import type { Artist } from "~/models/domain/artist";

const { artist } = defineProps<{
    artist: Artist | undefined;
}>();
const artistLink = ref(artist ? `/artists/${artist.id}` : null);
</script>
<template>
    <Item
        fit-image-to-placeholder-ratio
        image-type="poster"
        :title="artist?.name"
        :href="artistLink"
        :image="artist?.poster"
        :secondary-title="null"
        :secondary-href="null"
    />
</template>
