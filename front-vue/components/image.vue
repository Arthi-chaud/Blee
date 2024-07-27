<!-- eslint-disable vue/multi-word-component-names -->
<script setup lang="ts">
import type { Image, ImageType } from "~/models/domain/image";
import { ref } from "vue";
const { image, imageType } = defineProps<{
    image: Image | undefined | null;
    imageType: ImageType;
    disableBorderRadius?: boolean;
    fitToExpectedAspectRatio?: true;
}>();

const blurashURL = computed(() => blurHashToDataURL(image?.blurhash));
const aspectRatio = imageType == "poster" ? 2 / 3 : 16 / 9;
const imageIsLoaded = ref(false);
</script>
<template>
    <div
        :class="{
            // We set this to avoid shift while image is loading
            'h-full': !image || !imageIsLoaded,
            'w-full': !image || !imageIsLoaded,
        }"
        :style="{
            aspectRatio,
        }"
    >
        <div
            class="poster-rounded flex align-end justify-center h-full w-full relative"
            :style="
                !fitToExpectedAspectRatio
                    ? {
                          aspectRatio: image?.aspect_ratio,
                      }
                    : {}
            "
        >
            <div
                v-if="image"
                :style="{
                    background: `url(${blurashURL})`,
                    backgroundSize: 'cover',
                }"
                class="h-full w-full"
            >
                <img
                    :src="'/api/images/' + image.id"
                    :style="{
                        opacity: isSSR() || imageIsLoaded ? 1 : 0,
                        transition: 'opacity 0.2s ease-in',
                    }"
                    class="h-full w-full object-cover"
                    @load="imageIsLoaded = true"
                />
            </div>
            <div
                v-else-if="image === undefined"
                :style="{
                    aspectRatio,
                }"
                class="poster-skeleton w-full h-full"
            />
            <div
                v-else-if="image === null"
                :style="{
                    aspectRatio,
                }"
                class="w-full h-full bg-base-300"
            />
        </div>
    </div>
</template>
