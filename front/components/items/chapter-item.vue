<script setup lang="ts">
import type { Chapter } from "~/models/domain/chapter";

const props = defineProps<{
    chapter: Chapter | undefined;
}>();
const playbackLink = ref(
    props.chapter
        ? `/player/movie:${props.chapter.movie_id}?start=${props.chapter.start_time}`
        : null,
);
const secondaryTitle = computed(() =>
    props.chapter
        ? formatDuration(props.chapter.end_time - props.chapter.start_time)
        : undefined,
);
</script>
<template>
    <Item
        fit-image-to-placeholder-ratio
        image-type="thumbnail"
        :title="chapter?.name"
        :href="playbackLink"
        :image="chapter?.thumbnail"
        :secondary-title="secondaryTitle"
        :secondary-href="null"
    />
</template>
