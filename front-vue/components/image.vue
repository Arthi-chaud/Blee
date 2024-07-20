<script setup lang="ts">
import type { Image } from "~/models/domain/image";
import { ref } from "vue";
const { image } = defineProps<{
    expectedAspectRatio: number;
    image: Image | undefined | null;
    disableBorderRadius?: boolean;
    fitToExpectedAspectRatio?: true;
}>();

// If SSR, we wont receive event 'on loaded'.
// In that case, we'll consider the image to be loaded already
const imageIsLoaded = ref(isSSR() ? true : false);
const blurashURL = computed(() =>
    isSSR() ? null : blurHashToDataURL(image?.blurhash),
);
</script>
<template>
    <div
        :class="{ [`aspect-${expectedAspectRatio}`]: true }"
        class="w-full h-full flex align-center justify-center"
    >
        <div
            class="poster-rounded"
            :style="{
                aspectRatio: image?.aspect_ratio,
            }"
        >
            <template v-if="image">
                <img
                    :class="{ opacity: imageIsLoaded ? 1 : 0 }"
                    :src="'/api/images/' + image.id"
                    :style="{
                        transition: 'opacity 0.5s linear',
                    }"
                    @load="() => (imageIsLoaded = true)"
                />
                <img
                    v-if="blurashURL && !imageIsLoaded"
                    :src="blurashURL"
                    class="h-full w-full"
                />
                <div
                    class="h-full w-full"
                    :style="{ backgroundColor: image?.colors?.at(0) }"
                />
            </template>
            <div v-else class="poster-skeleton w-full h-full"></div>
        </div>
    </div>
</template>
