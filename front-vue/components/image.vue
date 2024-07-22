<script setup lang="ts">
import type { Image } from "~/models/domain/image";
import { ref, type VNodeRef } from "vue";
const { image } = defineProps<{
    expectedAspectRatio: number;
    image: Image | undefined | null;
    disableBorderRadius?: boolean;
    fitToExpectedAspectRatio?: true;
}>();

const blurashURL = computed(() => blurHashToDataURL(image?.blurhash));

const imageIsLoaded = ref(false);
</script>
<template>
    <div
        :class="{
            [`aspect-[${expectedAspectRatio}]`]: true,
            'h-full': !image,
            'w-full': !image,
        }"
    >
        <div
            class="poster-rounded flex align-end justify-center h-full w-full relative"
            :style="{
                aspectRatio: image?.aspect_ratio,
            }"
        >
            <template v-if="image">
                <img
                    :src="'/api/images/' + image.id"
                    :style="{
                        opacity: isSSR() || imageIsLoaded ? 1 : 0,
                        transition: 'opacity 0.2s ease-in',
                    }"
                    @load="imageIsLoaded = true"
                />
                <img
                    v-if="blurashURL"
                    :src="blurashURL"
                    id="blurhashElemId"
                    :style="{ zIndex: -1 }"
                    class="h-full w-full absolute top-0"
                />
            </template>
            <div
                v-else-if="image === undefined"
                class="poster-skeleton w-full h-full"
            ></div>
            <div
                v-else-if="image === null"
                class="w-full h-full bg-base-300"
            ></div>
        </div>
    </div>
</template>
