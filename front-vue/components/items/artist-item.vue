<script setup lang="ts">
import type { Artist } from "~/models/domain/artist";
import type { Package } from "~/models/domain/package";

const { artist } = defineProps<{
    artist: Artist | undefined;
}>();
const artistLink = ref(
    artist ? `/artists/${artist.id}` : undefined,
);
</script>
<template>
    <Item
        :title="artist?.name"
        :href="artistLink"
        :image="artist?.poster"
        :secondaryTitle="null"
    />
</template>
