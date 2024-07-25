<script setup lang="ts">
import type { Package } from "~/models/domain/package";

const props = defineProps<{
    package: Package | undefined;
    secondaryHref?: string | null;
    formatSecondaryTitle?: (p: Package) => string;
}>();
const packageLink = ref(
    props.package ? `/packages/${props.package?.id}` : null,
);
const secondaryHref = computed(() =>
    props.secondaryHref === null
        ? null
        : props.secondaryHref ??
          (props.package ? `/artists/${props.package?.artist_id}` : null),
);
const secondaryTitle = computed(() =>
    props.package
        ? props.formatSecondaryTitle?.(props.package) ??
          props.package?.artist_name ??
          undefined
        : undefined,
);
</script>
<template>
    <Item
        imageType="poster"
        :title="package?.name"
        :href="packageLink"
        :image="package?.poster"
        :secondaryTitle="secondaryTitle"
        :secondaryHref="secondaryHref"
    />
</template>
