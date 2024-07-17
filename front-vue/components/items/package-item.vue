<script setup lang="ts">
import type { Package } from "~/models/domain/package";

const props = defineProps<{
    package: Package | undefined;
}>();
</script>
<template>
    <Image
        :image="props.package?.poster"
        :expectedAspectRatio="3 / 4"
        :fitToExpectedAspectRatio="false"
    />
</template>
