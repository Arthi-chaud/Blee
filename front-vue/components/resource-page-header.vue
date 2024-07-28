<script setup lang="ts">
import type { Image as ImageType } from "~/models/domain/image";

defineProps<{
    resourceName: string | undefined;
    poster: ImageType | undefined | null;
    brief: string | null | undefined;
}>();
</script>

<template>
    <div class="max-w-screen-md w-full">
        <div
            class="gap-x-6 xs:grid grid-cols-1 md:flex flex-col md:flex-row"
            :style="{ aspectRatio: 16 / 9, height: 'fit-content' }"
        >
            <div class="md:flex-1 aspect-video md:aspect-auto flex">
                <div class="h-full w-full flex items-center justify-center">
                    <Image :image="poster" image-type="poster" />
                </div>
            </div>
            <div
                class="md:flex-2 flex flex-col justify-evenly items-center md:items-start pt-2 md:pt-0"
            >
                <div />
                <p
                    v-if="resourceName"
                    class="header-text prose-xl line-clamp-2 break-all w-full text-center md:text-start"
                >
                    {{ resourceName }}
                </p>
                <div v-else class="header-text skeleton h-7 w-20" />
                <div class="header-text"><slot name="secondary"></slot></div>
                <div class="header-text"><slot name="ternary"></slot></div>
                <div />
            </div>
        </div>
        <div class="mt-4 mb-4">
            <p
                v-if="brief"
                onclick="brief_modal.showModal()"
                class="line-clamp-4 font-light break-all w-full cursor-pointer"
            >
                {{ brief }}
            </p>
            <div v-if="brief === undefined" class="skeleton h-4 w-full" />
            <dialog id="brief_modal" class="modal">
                <div class="modal-box max-w-3xl">
                    <h3 class="text-lg font-bold">{{ resourceName }}</h3>
                    <p class="py-4">{{ brief }}</p>
                </div>
            </dialog>
        </div>
    </div>
</template>
<style>
.header-text {
    @apply pt-2;
    @apply md:pt-0;
}
</style>
