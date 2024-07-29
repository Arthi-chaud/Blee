<script setup lang="ts">
import type { Image as ImageModel } from "~/models/domain/image";

defineProps<{
    canGoBack: boolean;
    poster: ImageModel | undefined | null;
    title: string | undefined;
    subtitle: string | undefined | null;
    onBackButtonTap: () => void;
}>();
</script>
<template>
    <div class="w-full h-full z-20 fixed">
        <div class="top-0 h-20 flex justify-start m-1">
            <button class="btn btn-ghost" @click="onBackButtonTap()">
                <fa
                    :icon="canGoBack ? 'arrow-left' : 'home'"
                    class="prose-lg text-white"
                />
            </button>
        </div>
        <div
            class="w-full absolute bottom-0 h-[140px] controls flex p-3 flex-row"
        >
            <Image :image="poster" image-type="poster" />
            <div class="w-full flex flex-col justify-between mt-7 pl-3">
                <p v-if="title !== undefined" class="controls-text prose-md">{{ title }}</p>
                <div v-else class="skeleton h-4 w-20"/>
                <p v-if="subtitle !== undefined" class="controls-text prose-sm">{{ subtitle }}</p>
                <div v-else class="skeleton h-3 w-20"/>
                <div/>
            </div>
        </div>
    </div>
</template>
<style>
.controls-text {
    color: white;
}
.controls {
    background-image: linear-gradient(transparent, black);
}
</style>
