<script setup lang="ts">
import type { Artist } from "~/models/domain/artist";

const { artist } = defineProps<{
    artist: Artist | undefined;
}>();
const artistLink = ref(artist ? `/artists/${artist.id}` : null);
</script>
<template>
    <Item
        fitImageToPlaceholderRatio
        imageType="poster"
        :title="artist?.name"
        :href="artistLink"
        :image="artist?.poster"
        :secondaryTitle="null"
        :secondaryHref="null"
    />
</template>
