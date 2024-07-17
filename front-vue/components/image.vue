<script setup lang="ts">
import type { Image } from "~/models/domain/image";
import { ref } from "vue";

// If SSR, we wont receive event 'on loaded'.
// In that case, we'll consider the image to be loaded already
const imageIsLoaded = ref(isSSR() ? true : false);

defineProps<{
    expectedAspectRatio: number;
    image: Image | undefined | null;
    disableBorderRadius?: boolean;
    fitToExpectedAspectRatio: boolean;
}>();
</script>
<template>
    <div
        class="aspect-[3/4] w-full h-full flex align-items-center justify-content-center"
    >
        <div
            :style="{
                aspectRatio: image?.aspect_ratio,
                objectFit: 'contain',
                display: 'block',
                height: '100%',
            }"
        >
            <template v-if="image">
                <img
                    :src="'/api/images/' + image.id"
                    @load="imageIsLoaded = true"
                    class="h-full w-full object-contain"
                />
                //TODO: blurhash + fade in animation
            </template>
            <div v-else class="skeleton w-full h-full"></div>
        </div>
    </div>
</template>
