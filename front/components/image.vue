<!-- eslint-disable vue/multi-word-component-names -->
<script setup lang="ts">
import type { Image, ImageType } from "~/models/domain/image";
import { ref } from "vue";
import { API } from "~/api/api";
const props = defineProps<{
    image: Image | undefined | null;
    imageType: ImageType;
    fitToExpectedAspectRatio?: true;
    //TODO for skeleton
    disableAspectRatio?: true;
}>();

//TODO Blurhahs is undefined?
const blurashURL = computed(() => blurHashToDataURL(props.image?.blurhash));
const aspectRatio = props.imageType == "poster" ? 2 / 3 : 16 / 9;
const imageIsLoaded = ref(false);
</script>
<template>
    <div
        class="flex items-center justify-center w-auto object-contain overflow-hidden max-h-full poster-rounded"
        :class="{
            'h-full': image === null || image === undefined,
        }"
        :style="{
            aspectRatio: !fitToExpectedAspectRatio
                ? (image?.aspect_ratio ?? aspectRatio)
                : aspectRatio,
        }"
    >
        <div
            v-if="image === undefined"
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
            class="poster-rounded w-full h-full bg-base-300"
        />
        <div
            v-else
            class="poster-rounded"
            :style="{
                aspectRatio: image?.aspect_ratio,
                background: `url(${blurashURL})`,
                backgroundSize: 'cover',
            }"
        >
            <img
                :src="API.buildImageUrl(image.id)"
                :style="{
                    opacity: isSSR() || imageIsLoaded ? 1 : 0,
                    transition: 'opacity 0.2s ease-in',
                }"
                class="h-full w-full object-cover poster-rounded"
                @load="imageIsLoaded = true"
            />
        </div>
    </div>
</template>
