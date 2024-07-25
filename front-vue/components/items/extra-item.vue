<script setup lang="ts">
import type { Extra } from "~/models/domain/extra";

const props = defineProps<{
    extra: Extra | undefined;
    secondaryHref?: string | null;
    formatSecondaryTitle?: (e: Extra) => string;
}>();
const playbackLink = ref(
    props.extra ? `/player/extra:${props.extra.id}` : null,
);
const secondaryTitle = computed(() =>
    props.extra
        ? props.formatSecondaryTitle?.(props.extra) ??
          props.extra?.artist_name ??
          undefined
        : undefined,
);
const secondaryHref = computed(() =>
    props.secondaryHref === null
        ? null
        : props.secondaryHref ??
          (props.extra ? `/artists/${props.extra?.artist_id}` : null),
);
</script>
<template>
    <Item
        fitImageToPlaceholderRatio
        imageType="thumbnail"
        :title="extra?.name"
        :href="playbackLink"
        :image="extra?.thumbnail"
        :secondaryTitle="secondaryTitle"
        :secondaryHref="secondaryHref"
    />
</template>
